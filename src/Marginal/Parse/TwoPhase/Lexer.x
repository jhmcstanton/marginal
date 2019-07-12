{
{-# LANGUAGE OverloadedStrings #-}
module Marginal.Parse.TwoPhase.Lexer
  (
    alexScanTokens,
    mtoken,
    posn,
    AlexPosn(..),
    MarginalToken(..),
    Token(..)
  )
  where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "posn-bytestring"

tokens :-

  \                    { token TSpace   }
  \t                   { token TTab     }
  \n                   { token TNewline }
  ~[\ \t\n]            ;

{

token :: MarginalToken -> AlexPosn -> ByteString -> Token
token m p _ = Token p m

data Token = Token AlexPosn MarginalToken deriving (Show)

mtoken (Token _ tok) = tok
posn   (Token p _  ) = p

data MarginalToken = TSpace | TTab | TNewline deriving (Eq, Show)

}
