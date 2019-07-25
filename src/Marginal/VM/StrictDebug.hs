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

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.List (zipWith4)
import qualified Data.IntMap.Strict as I
import           Data.Text (Text)
import qualified Data.Text    as T
import           Data.Text.IO hiding (getLine)
import qualified Data.Vector  as V
import           Prelude hiding (cycle, putStr, putStrLn)
import           System.Console.ANSI

import           Marginal.Parse.Type
import           Marginal.VM.Type
import           Marginal.VM.Strict

data EventType = Quit | Next | Up | Down | Labels

data VMStrictDebug = VMStrictDebug {
   vm        :: VMStrict,
   cycle     :: Integer,
   windowPos :: Int, -- Starting line of window being displayed (frequently 0)
   output    :: Text
 }

instance VM VMStrictDebug where
  type VMOut VMStrictDebug = IO
  run v@VMStrictDebug{vm=vmInner} is =
    loop v{ vm=vmInner{ labels = locateLabels is}} is
    where
      loop v@VMStrictDebug{vm = vInner} is = do
        -- Header Stuff
        clearScreen
        setCursorPosition 0 0
        putStrLn "MarginalD: The Marginal Whitespace Debugger"
        putStrLn $ "Total Cycles: " <> (T.pack . show . cycle $ v) <> "\n"
        Just (height, width) <- getTerminalSize
        let colWidth = width `div` (length columns)
        mapM_ putStr . fmap (formatCol colWidth) $ columns
        putStrLn ""

        -- Create columns from state
        let splitOutLines =
              formatCol colWidth <$> T.chunksOf (colWidth - 1) (output v)
        let isToShow      = V.toList . V.take (height - 5) . V.drop (pc vInner) $ is
        let insOut        = (formatCol colWidth . showInst) <$> isToShow
        let stackOut      = (formatCol colWidth . T.pack . show) <$> stack vInner
        let heapList      = I.toList . heap $ vInner
        let heapOut       = (formatCol colWidth . T.pack . show) <$> heapList
        -- Print State
        let combinedCols =
              zipWith4 (\a b c d -> a <> b <> c <> d)
                (blankCols colWidth splitOutLines)
                (blankCols colWidth insOut)
                (blankCols colWidth stackOut)
                (blankCols colWidth heapOut)
        let colSize =
              max (length splitOutLines)
                (max (length insOut)
                  (max (length stackOut)
                       (length heapOut )))
        mapM_ putStrLn $ take colSize combinedCols
        getLine -- TODO make this use the actual keys
        -- Run the step
        let instr = is V.! (pc vInner)
        let v' = step v instr
        let vInner' = vm v'
        -- TODO: Abstract out some of the IO stuff here
        case stepResult vInner' of
          VMExit      -> pure v'
          VMOut l     -> loop v'{output = output v' <> T.pack l} is
          VMErr e     -> vmErr instr (pc vInner) e >> pure v'
          VMGetChar l -> do
            c <- getChar
            let vheap = heap vInner'
            let v'' = v'{
                  vm = vInner' {
                    heap = I.insert l (fromIntegral . ord $ c) vheap,
                    stepResult = VMOther
                    }
                  }
            loop v'' is
          VMGetNum  l -> do
            input <- getLine
            let vheap = heap vInner'
            let v'' = v'{
                  vm = vInner' {
                    heap = I.insert l (read input) vheap,
                    stepResult = VMOther
                    }
                  }
            loop v'' is
          _        -> loop v' is
        pure v

  step v@VMStrictDebug{vm, cycle} i =
    let vm' = step vm i in
    v{vm = vm', cycle = cycle + 1}
  start = VMStrictDebug start 0 0 mempty

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
