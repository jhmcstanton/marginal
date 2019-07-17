{
{-# LANGUAGE OverloadedStrings #-}
module Marginal.Parse.OnePhase.Parser
  (
    scanInstructions,
  )
  where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import           Marginal.Parse.Type
}

%wrapper "posn-bytestring"

@digit  = \ |\t
@number = @digit@digit+\n
@label  = @digit+\n

-- All Instruction Modification Parameters
@stack  = \ 
@arith  = \t\ 
@heap   = \t\t
@flow   = \n
@io     = \t\n

tokens :-

  @stack\ @number      { numberToInstr Push     }
  @stack\n\            { instr Dup              }
  @stack\n\t           { instr Swap             }
  @stack\n\n           { instr Drop             }

  @arith\ \            { instr Add              }
  @arith\ \t           { instr Sub              }
  @arith\ \n           { instr Mult             }
  @arith\t\            { instr Div              }
  @arith\t\t           { instr Mod              }

  @heap\               { instr Store            }
  @heap\t              { instr Retrieve         }

  @flow\ \ @label      { labelToInstr Mark      }
  @flow\ \t@label      { labelToInstr Func      }
  @flow\ \n@label      { labelToInstr Jump      }
  @flow\t\ @label      { labelToInstr JumpZero  }
  @flow\t\t@label      { labelToInstr JumpNeg   }
  @flow\t\n            { instr Return           }
  @flow\n\n            { instr Exit             }

  @io\ \               { instr PrintChar        }
  @io\ \n              { instr PrintNum         }
  @io\t\               { instr ReadChar         }
  @io\t\t              { instr ReadNum          }

{

-- Main token scanner. Removes all characters besides
-- ' ', '\t', and '\n' prior to running alex
scanInstructions :: ByteString -> [Instruction]
scanInstructions = alexScanTokens . BS.filter whitespaceChar where
  whitespaceChar c = c == ' ' || c == '\t' || c == '\n'

numberToInstr :: (Number -> Instruction)
               -> a
               -> ByteString
               -> Instruction
numberToInstr f _ input = f . readNumber $ input

labelToInstr :: (Label -> Instruction)
             -> a
             -> ByteString
             -> Instruction
labelToInstr f _ = f . Label . BS.drop 3 . BS.init

digit :: Char -> Integer
digit ' '  = 0
digit '\t' = 1

sign :: Char -> Integer
sign ' '  = 1
sign '\t' = -1

readInteger :: ByteString -> Integer
readInteger bs = bsign * BS.foldl combine 0 userDigits where
  userDigits = BS.init . BS.dropWhile (== ' ') . BS.drop 3 $ bs
  bsign = sign (bs `BS.index` 2)
  combine acc c = acc * 2 + (digit c)

readNumber :: ByteString -> Number
readNumber = Number . readInteger

instr :: Instruction -> a -> b -> Instruction
instr i _ _ = i
}
