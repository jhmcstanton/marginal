{-# Language DataKinds        #-}
{-# Language GADTs            #-}
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

data RunOptions = RunOptions
  {
    file   :: FilePath
  , parser :: String -- ParseType
  -- , vm     :: EvalStrategy -- TODO: add this*
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
  -- <*> strOption (  long "vm"
  --               <> metavar "[VM]"
  --               <> help "Virtual machine to execute instructions"
  --               <> showDefault
  --               <> value Strict
  --             )

main :: IO ()
main = execParser opts >>= runProg where
  opts = info (optionParser <**> helper)
         ( fullDesc
         <> header "Marginal: The Whitespace Interpreter"
         <> progDesc desc)
  desc = "Runs whitespace programs - complete with pretty bad error messages"

runProg :: RunOptions -> IO ()
runProg (RunOptions file parser) = do
  f <- B.readFile file
  case readMaybe parser of
    Nothing -> putStrLn "Please choose a parser from: [Strict|Lazy]"
    Just p  -> do
      let parse        = pickParser p
      let instructions = parse f
      run strictStart instructions
      pure ()

pickParser :: ParseType -> B.ByteString -> V.Vector Instruction
pickParser t = V.fromList . pick t where
  pick AlexOnly  = scanInstructions
  pick AlexHappy = parse . alexScanTokens
