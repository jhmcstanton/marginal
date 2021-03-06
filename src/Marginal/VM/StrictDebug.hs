{-# Language ConstraintKinds   #-}
{-# Language DataKinds         #-}
{-# Language NamedFieldPuns    #-}
{-# Language OverloadedStrings #-}
{-# Language Strict            #-}
{-# Language TypeFamilies      #-}
module Marginal.VM.StrictDebug
  (
    run,
    step,
    start,
    -- VMStrictDebug(..) -- replaced by getters below
    vm,
    state
  )
where

import           Control.Applicative
import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.Foldable (fold)
import           Data.HashMap.Strict
import qualified Data.IntMap.Strict as I
import           Data.List (maximum, zipWith4)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
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
  | Break Int
  | Up
  | Down
  | Labels
  | Retry -- Bad input
  deriving Show

data AwaitInput = Await | Skip deriving Show


data DebuggerState = DebuggerState {
    wait         :: AwaitInput,
    output       :: Text,
    windowPos    :: Int, -- Starting line of window being displayed (frequently 0)
    cycle        :: Integer,
    windowHeight :: Int,
    windowWidth  :: Int,
    breakPoints  :: Set Int
  }

setAwait :: DebugState m => AwaitInput -> m ()
setAwait w = modify $ \s -> s{ wait = w }

getAwait :: DebugState m => m AwaitInput
getAwait = wait <$> get

adjustWinPos :: DebugState m => Int -> m ()
adjustWinPos i = modify $ \s -> s{ windowPos = windowPos s + i }

getWinPos :: DebugState m => m Int
getWinPos = windowPos <$> get

appendOutput :: DebugState m => Text -> m ()
appendOutput o = modify $ \s -> s{ output = output s <> o }

getOutput :: DebugState m => m Text
getOutput = output <$> get

incCycle :: DebugState m => m ()
incCycle = modify $ \s -> s{ cycle = cycle s + 1}

getCycle :: DebugState m => m Integer
getCycle = cycle <$> get

getWinHeight :: DebugState m => m Int
getWinHeight = windowHeight <$> get

getWinWidth :: DebugState m => m Int
getWinWidth = windowWidth <$> get

setWindowSize :: (MonadIO m, DebugState m) => m (Int, Int)
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

getBreakPoints :: DebugState m => m (Set Int)
getBreakPoints = breakPoints <$> get

addBreakPoint :: DebugState m => Int -> m ()
addBreakPoint n = modify $ \s -> s{ breakPoints = S.insert n (breakPoints s) }

type DebugState m = (StateType m ~ DebuggerState, MonadState m)

instance VM 'StrictDebug where
  type VMOut 'StrictDebug   = IO
  data VMState 'StrictDebug = VMStrictDebug {
   vm        :: VMStrict',
   state     :: DebuggerState
  }
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
      loop :: (MonadIO m, DebugState m)
           => VMStrict'
           -> V.Vector Instruction
           -> m VMStrict'
      loop vm is = do
        -- Header Stuff
        liftIO refreshScreen
        cycle <- getCycle
        liftIO $ putStrLn $ "Total Cycles: " <> (T.pack . show $ cycle) <> "\n"
        (height, width) <- setWindowSize
        let colWidth = width `div` (length columns)

        -- Create columns from state
        out <- getOutput
        let chunkOut :: Char -> (Text, [Text]) -> (Text, [Text])
            chunkOut c (acc, accs) = if T.length acc == colWidth
                                     then (T.singleton c, acc : accs)
                                     else (acc `T.snoc` c, accs)
        let chunkedLines  = reverse . snd . T.foldr chunkOut (mempty, []) $ out
        let splitOutLines = formatCol colWidth <$> chunkedLines
        let isToShow      = V.toList . V.drop (pc vm) $ is
        let insOut        = zipWith showInstCol [pc vm..] isToShow
        let stackOut      = (T.pack . show) <$> stack vm
        let heapList      = I.toList . heap $ vm
        let heapOut       = (T.pack . show) <$> heapList
        -- Print State
        let rawCols = (:) <$> ZipList columns
                          <*> ZipList [splitOutLines, insOut, stackOut, heapOut]
        let cols = (ZipList . blankCols colWidth . fmap (formatCol colWidth))
                   <$> getZipList rawCols
        let combinedCols =
              (\a b c d -> a <> b <> c <> d)
              <$> cols !! 0
              <*> cols !! 1
              <*> cols !! 2
              <*> cols !! 3
        liftIO . mapM_ putStrLn $ take (height - 3) $ getZipList combinedCols
        breakPoints <- getBreakPoints
        if pc vm `S.member` breakPoints
        then setAwait Await
        else pure ()
        w  <- getAwait
        ev <- liftIO $ getInput w
        case ev of
          Quit     -> pure vm
          Next     ->                  stepInner vm is
          Continue -> setAwait Skip >> stepInner vm is
          Retry    -> loop vm is
          Labels   -> (liftIO $ printLabels vm) >> loop vm is
          Break i  -> addBreakPoint i           >> loop vm is
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

  start = VMStrictDebug start (DebuggerState Await mempty 0 0 30 80 mempty)

getInput :: AwaitInput -> IO EventType
getInput Skip  = pure Continue
getInput Await = do
  c <- getChar
  case c of
    'n'    -> pure Next
    'c'    -> pure Continue
    'q'    -> pure Quit
    'l'    -> pure Labels
    'b'    -> (Break . read) <$> getLine
    '\ESC' -> do
      _    <- getChar
      code <- getChar
      case code of
        'A' -> pure Up
        'B' -> pure Down
        _   -> pure Retry
    _       -> pure Retry

refreshScreen :: IO ()
refreshScreen = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "MarginalD: The Marginal Whitespace Debugger"

printLabels :: VMStrict' -> IO ()
printLabels vm = do
  refreshScreen
  putStrLn "Labels: "
  mapM_ putStrLn strictls
  c <- getChar
  if c `telem` "ncl\ESC"
  then pure ()
  else printLabels vm
  where
    exitChars = "ncl\ESC"
    ls = toList . labels $ vm
    strictls = fmap (showLabel . fst) ls
    telem c t = case T.findIndex (== c) t of
                  Just _  -> True
                  Nothing -> False

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
  let text' = T.take (totalWidth - 1) text in
  T.take totalWidth $ text' <> T.replicate (totalWidth - T.length text') " "

columns :: [Text]
columns =
    [
      "Stdout",
      "Instructions",
      "Stack",
      "Heap"
    ]

showInstCol :: Int -> Instruction -> Text
showInstCol i c = (T.pack $ show i) <> ": " <> showInst c

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
