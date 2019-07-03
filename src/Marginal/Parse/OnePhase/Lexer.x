{
{-# LANGUAGE OverloadedStrings #-}
module Marginal.Parse.OnePhase.Lexer
  (
    scanTokens,
    AlexPosn(..),
    MarginalToken(..),
    Token(..)
  )
  where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
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

  @stack\ @number      { numberToToken TPush    }
  @stack\n\            { token TDup             }
  @stack\n\t           { token TSwap            }
  @stack\n\n           { token TDrop            }

  @arith\ \            { token TAdd             }
  @arith\ \t           { token TSub             }
  @arith\ \n           { token TMult            }
  @arith\t\            { token TDiv             }
  @arith\t\t           { token TMod             }

  @heap\               { token TStore           }
  @heap\t              { token TRetrieve        }

  @flow\ \ @label      { labelToToken TMark     }
  @flow\ \t@label      { labelToToken TFunc     }
  @flow\ \n@label      { labelToToken TJump     }
  @flow\t\ @label      { labelToToken TJumpZero }
  @flow\t\t@label      { labelToToken TJumpNeg  }
  @flow\t\n            { token TReturn          }
  @flow\n\n            { token TExit            }

  @io\ \               { token TPrintChar       }
  @io\ \n              { token TPrintNum        }
  @io\t\               { token TReadChar        }
  @io\t\t              { token TReadNum         }

{

-- Main token scanner. Removes all characters besides
-- ' ', '\t', and '\n' prior to running alex
scanTokens :: ByteString -> [Token]
scanTokens = alexScanTokens . BS.filter whitespaceChar where
  whitespaceChar c = c == ' ' || c == '\t' || c == '\n'

numberToToken :: (Number -> MarginalToken)
               -> AlexPosn
               -> ByteString
               -> Token
numberToToken f posn input = Token posn (f . readNumber $ input)

labelToToken :: (Label -> MarginalToken)
             -> AlexPosn
             -> ByteString
             -> Token
labelToToken f posn input = Token posn (f . Label . BS.init $ input)

digit :: Char -> Integer
digit ' '  = 0
digit '\t' = 1

sign :: Char -> Integer
sign ' '  = 1
sign '\t' = -1

readInteger :: ByteString -> Integer
readInteger bs = bsign * BS.foldl combine 0 userDigits where
  userDigits = BS.dropWhile (== ' ') (BS.init $ BS.tail bs)
  bsign = sign (BS.head bs)
  combine acc c = acc * 2 + (digit c)

readNumber :: ByteString -> Number
readNumber = Number . readInteger

token :: MarginalToken -> AlexPosn -> ByteString -> Token
token m p _ = Token p m

data Token = Token AlexPosn MarginalToken
  deriving (Show)

newtype Number = Number Integer deriving (Show)
newtype Label  = Label ByteString deriving (Show)

data MarginalToken =
  -- Stack Manipulation
    TPush Number
  | TDup
  | TSwap
  | TDrop
  -- Arithmetic
  | TAdd
  | TSub
  | TMult
  | TDiv
  | TMod
  -- Heap Access
  | TStore
  | TRetrieve
  -- Flow Control
  | TMark Label
  | TFunc Label
  | TJump Label
  | TJumpZero Label
  | TJumpNeg  Label
  | TReturn
  | TExit
  -- IO
  | TPrintChar
  | TPrintNum
  | TReadChar
  | TReadNum
  deriving (Show)
}
