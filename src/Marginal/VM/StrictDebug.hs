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

import           Data.Char
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
        Just (height, width) <- getTerminalSize
        let colWidth = width `div` (length columns)
        mapM_ putStr . fmap (formatCol colWidth) $ columns
        putStrLn $ "\n\nTotal Cycles: " <> (T.pack . show . cycle $ v) <> "\n"

        -- Print State
        let splitOutLines = T.chunksOf (colWidth - 1) (output v)
        -- TODO Actually print everything and concat columns
        mapM_ putStrLn splitOutLines
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

formatCol :: Int -> Text -> Text
formatCol totalWidth text = text <> T.replicate (totalWidth - T.length text) " "

columns :: [Text]
columns =
    [
      "Stdout",
      "Instructions",
      "Stack",
      "Heap"
    ]

legend :: [(String, String)]
legend =
  [
    ("n", "next"),
    ("l", "labels"),
    ("up", "scroll up"),
    ("down", "scroll down"),
    ("q", "quit")
  ]
