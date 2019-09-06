{-# Language DataKinds        #-}
{-# Language NamedFieldPuns   #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies     #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Data.Proxy
import           Data.Semigroup ((<>))
import qualified Data.Vector as V
import           Options.Applicative
import           Text.Read (readMaybe)

import           Marginal.Parse.Type
import           Marginal.Parse.OnePhase.Parser
import           Marginal.Parse.TwoPhase.Lexer
import           Marginal.Parse.TwoPhase.Parser
import           Marginal.VM.Type
import           Marginal.VM.Strict
import           Marginal.VM.StrictDebug

data RunOptions = RunOptions
  {
    file             :: FilePath
  , parser           :: String -- ParseType
  , vmType           :: String -- VMType
  , dumpLex          :: Bool
  , dumpInstructions :: Bool
  }
-- * Figure out how to get typeclasses to choose the appropriate vm

data ParseType = AlexOnly | AlexHappy deriving (Read, Show)

optionParser :: Parser RunOptions
optionParser = RunOptions
  <$> argument str (metavar "FILE")
  <*> strOption (  long "parser"
                <> short 'p'
                <> metavar "[PARSER TYPE]"
                <> help "Parser for instructions"
                <> showDefault
                <> value (show AlexHappy)
                )
  <*> strOption (  long "vm"
                <> metavar "[VM]"
                <> help "Virtual machine to execute instructions"
                <> showDefault
                <> value (show Strict)
              )
  <*> switch (  long "dump-lex"
              <> short 'l'
              <> help "Dumps the result of lexing"
              <> showDefault
             )
  <*> switch (  long "dump-instructions"
             <> short 'i'
             <> help "Dumps the parsed instructions"
             <> showDefault
             )

main :: IO ()
main = execParser opts >>= runProg where
  opts = info (optionParser <**> helper)
         ( fullDesc
         <> header "Marginal: The Whitespace Interpreter"
         <> progDesc desc)
  desc = "Runs whitespace programs - complete with pretty bad error messages"

parseOptionError = putStrLn "Please choose a parser from: [Alex|AlexHappy]"
vmOptionError    = putStrLn "Please choose a VM from: [Strict|VMStrict]"

runProg :: RunOptions -> IO ()
runProg opts@RunOptions{file, parser, vmType} = do
  f <- B.readFile file
  case (readMaybe parser, readMaybe vmType) of
    (Nothing, Nothing)   -> parseOptionError >> vmOptionError
    (Nothing, _)         -> parseOptionError
    (_, Nothing)         -> vmOptionError
    (Just p, Just vType) -> do
      let parse        = pickParser p
      let instructions = parse f
      case (dumpLex opts, dumpInstructions opts) of
        (True, True)   -> printLex p f >> printParse instructions
        (True, False)  -> printLex p f
        (False, True)  -> printParse instructions
        (False, False) ->
          case vType of
            Strict -> run (start :: VMState 'Strict) instructions >> pure ()
            StrictDebug -> run (start :: VMState 'StrictDebug) instructions >> pure ()


pickParser :: ParseType -> B.ByteString -> V.Vector Instruction
pickParser t = V.fromList . pick t where
  pick AlexOnly  = scanInstructions
  pick AlexHappy = parse . alexScanTokens

printLex :: ParseType -> B.ByteString -> IO ()
printLex p bs =
  case p of
    AlexOnly  ->
      printHeader ("Lex Results (same as parse results for this parser)")
      >> (mapM_ (putStrLn . show) $ scanInstructions bs)
    AlexHappy ->
      printHeader "Lex Results"
      >> (mapM_ (putStrLn . show) . alexScanTokens $ bs)

printParse :: V.Vector Instruction -> IO ()
printParse is = do
  printHeader "Parse Results "
  mapM_ (putStrLn . show) is

printHeader :: String -> IO ()
printHeader h = do
  bars
  putStrLn $ "== " ++ h
  bars
  where bars = putStrLn $ replicate (4 + length h) '='
