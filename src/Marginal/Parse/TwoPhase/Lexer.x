{
{-# LANGUAGE OverloadedStrings #-}
module Marginal.Parse.TwoPhase.Lexer
  (
    scanSimpleTokens,
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

scanSimpleTokens = alexScanTokens

token :: MarginalToken -> AlexPosn -> ByteString -> Token
token m p _ = Token p m

data Token = Token AlexPosn MarginalToken deriving (Show)

data MarginalToken = TSpace | TTab | TNewline deriving (Show)
}
