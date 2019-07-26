{-# Language ConstraintKinds   #-}
{-# Language NamedFieldPuns    #-}
{-# Language OverloadedStrings #-}
{-# Language Strict            #-}
{-# Language TypeFamilies      #-}
module Marginal.VM.StrictDebug
  (
    run,
    step,
    start,
    VMStrictDebug(..)
  )
where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import qualified Data.IntMap.Strict as I
import           Data.List (maximum, zipWith4)
import           Data.Text (Text)
import qualified Data.Text    as T
import           Data.Text.IO hiding (getLine)
import qualified Data.Vector  as V
import           Prelude hiding (cycle, putStr, putStrLn)
import           System.Console.ANSI
import           System.IO (hGetBuffering, hSetBuffering, stdin, BufferMode(..))

import           Marginal.Parse.Type
import           Marginal.VM.Type
import           Marginal.VM.Strict

data EventType =
    Quit
  | Next
  | Continue
  | Up
  | Down
  | Labels
  | Retry -- Bad input
  deriving Show

data AwaitInput = Await | Skip

data VMStrictDebug = VMStrictDebug {
   vm        :: VMStrict,
   state     :: DebuggerState
 }

data DebuggerState = DebuggerState {
    wait         :: AwaitInput,
    output       :: Text,
    windowPos    :: Int, -- Starting line of window being displayed (frequently 0)
    cycle        :: Integer,
    windowHeight :: Int,
    windowWidth  :: Int
  }

setAwait :: VMState m => AwaitInput -> m ()
setAwait w = modify $ \s -> s{ wait = w }

getAwait :: VMState m => m AwaitInput
getAwait = wait <$> get

adjustWinPos :: VMState m => Int -> m ()
adjustWinPos i = modify $ \s -> s{ windowPos = windowPos s + i }

getWinPos :: VMState m => m Int
getWinPos = windowPos <$> get

appendOutput :: VMState m => Text -> m ()
appendOutput o = modify $ \s -> s{ output = output s <> o }

getOutput :: VMState m => m Text
getOutput = output <$> get

incCycle :: VMState m => m ()
incCycle = modify $ \s -> s{ cycle = cycle s + 1}

getCycle :: VMState m => m Integer
getCycle = cycle <$> get

getWinHeight :: VMState m => m Int
getWinHeight = windowHeight <$> get

getWinWidth :: VMState m => m Int
getWinWidth = windowWidth <$> get

setWindowSize :: (MonadIO m, VMState m) => m (Int, Int)
setWindowSize = do
  size <- liftIO getTerminalSize
  case size of
    Just (height, width) -> do
      modify $ \s -> s{ windowHeight = height, windowWidth = width }
      pure (height, width)
    Nothing -> do
      height <- getWinHeight
      width  <- getWinWidth
      pure (height, width)

type VMState m = (StateType m ~ DebuggerState, MonadState m)

instance VM VMStrictDebug where
  type VMOut VMStrictDebug = IO
  run VMStrictDebug{vm, state} is = do
    -- setup
    origBuffering <- hGetBuffering stdin
    hSetBuffering stdin NoBuffering
    -- main loop
    (v, s) <- runStateT (loop vm{ labels = locateLabels is} is) state
    -- tear down
    hSetBuffering stdin origBuffering
    pure $ VMStrictDebug v s
    where
      loop :: (MonadIO m, VMState m)
           => VMStrict
           -> V.Vector Instruction
           -> m VMStrict
      loop vm is = do
        -- Header Stuff
        liftIO clearScreen
        liftIO $ setCursorPosition 0 0
        liftIO $ putStrLn "MarginalD: The Marginal Whitespace Debugger"
        cycle <- getCycle
        liftIO $ putStrLn $ "Total Cycles: " <> (T.pack . show $ cycle) <> "\n"
        (height, width) <- setWindowSize
        let colWidth = width `div` (length columns)
        liftIO $ mapM_ putStr . fmap (formatCol colWidth) $ columns
        liftIO $ putStrLn ""

        -- Create columns from state
        out <- getOutput
        let splitOutLines =
              formatCol colWidth <$> T.chunksOf (colWidth - 1) out
        let isToShow      = V.toList . V.take (height - 5) . V.drop (pc vm) $ is
        let insOut        = (formatCol colWidth . showInst) <$> isToShow
        let stackOut      = (formatCol colWidth . T.pack . show) <$> stack vm
        let heapList      = I.toList . heap $ vm
        let heapOut       = (formatCol colWidth . T.pack . show) <$> heapList
        -- Print State
        let combinedCols =
              zipWith4 (\a b c d -> a <> b <> c <> d)
                (blankCols colWidth splitOutLines)
                (blankCols colWidth insOut)
                (blankCols colWidth stackOut)
                (blankCols colWidth heapOut)
        let colSize =
              maximum . fmap length $ [splitOutLines, insOut, stackOut, heapOut]
        liftIO $ mapM_ putStrLn $ take colSize combinedCols
        w <- getAwait
        ev <- liftIO $ getInput w
        case ev of
          Quit     -> pure vm
          Next     ->                  stepInner vm is
          Continue -> setAwait Skip >> stepInner vm is
          Retry    -> loop vm is
          _        -> loop vm is

      stepInner vm is = do
        -- Run the step
        let instr = is V.! (pc vm)
        let vm' = step vm instr
        incCycle
        -- TODO: Abstract out some of the IO stuff here
        case stepResult vm' of
          VMExit      -> pure vm'
          VMOut l     -> appendOutput (T.pack l) >> loop vm' is
          VMErr e     -> liftIO (vmErr instr (pc vm) e) >> pure vm'
          VMGetChar l -> do
            c <- liftIO $ getChar
            let vheap = heap vm'
            let vm'' = vm'{
                       heap = I.insert l (fromIntegral . ord $ c) vheap,
                       stepResult = VMOther
                     }
            loop vm'' is
          VMGetNum  l -> do
            input <- liftIO $ getLine
            let vheap = heap vm'
            let vm'' = vm' {
                       heap = I.insert l (read input) vheap,
                       stepResult = VMOther
                     }
            loop vm'' is
          _        -> loop vm' is

  step v@VMStrictDebug{vm, state} i =
    let vm' = step vm i in
    v{vm = vm', state = state{ cycle = cycle state + 1}}

  start = VMStrictDebug start (DebuggerState Await mempty 0 0 30 80)

getInput :: AwaitInput -> IO EventType
getInput Skip  = pure Continue
getInput Await = do
  c <- getChar
  case c of
    'n'    -> pure Next
    'c'    -> pure Continue
    'q'    -> pure Quit
    'l'    -> pure Labels
    '\ESC' -> do
      _    <- getChar
      code <- getChar
      case code of
        'A' -> pure Up
        'B' -> pure Down
        _   -> pure Retry
    _       -> pure Retry
vmErr :: Instruction -> Int -> String -> IO ()
vmErr i loc e = do
  putStrLn . T.pack $ "Error on instruction " <> show i <> " at " <> show loc
  putStrLn (T.pack e)

-- implemented in a later ansi-terminal release :(
getTerminalSize :: IO (Maybe (Int, Int))
getTerminalSize = do
  saveCursor
  setCursorPosition 10000 10000
  p <- getCursorPosition0
  restoreCursor
  pure p

blankCols :: Int -> [Text] -> [Text]
blankCols width cols = cols <> (repeat $ formatCol width mempty)

formatCol :: Int -> Text -> Text
formatCol totalWidth text =
  T.take (totalWidth ) $ text <> T.replicate (totalWidth - T.length text) " "

columns :: [Text]
columns =
    [
      "Stdout",
      "Instructions",
      "Stack",
      "Heap"
    ]

showInst :: Instruction -> Text
showInst o@(Mark l    ) = showLabelInstr o l
showInst o@(Func l    ) = showLabelInstr o l
showInst o@(Jump l    ) = showLabelInstr o l
showInst o@(JumpZero l) = showLabelInstr o l
showInst o@(JumpNeg l ) = showLabelInstr o l
showInst i = T.pack . show $ i

showLabelInstr :: Instruction -> Label -> Text
showLabelInstr i l = left <> showLabel l <> right where
  left = T.pack (takeWhile (/= ' ') $ show i) <> " "

  right = ")"
showLabel :: Label -> Text
showLabel (Label i) = "(Label " <> num <> ")" where
  num    = T.pack . show . foldl combine 0 $ digits
  combine acc c = acc * 2 + c
  digits = dropWhile (== 0) . fmap digit . B.unpack $ i
  digit ' '  = 0
  digit '\t' = 1

legend :: [(String, String)]
legend =
  [
    ("n", "next"),
    ("l", "labels"),
    ("up", "scroll up"),
    ("down", "scroll down"),
    ("q", "quit")
  ]
