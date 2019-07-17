{-# Language DeriveGeneric #-}
module Marginal.Parse.Type
  (
    Instruction(..),
    Label(..),
    Number(..)
  )
where

import           Data.ByteString.Lazy (ByteString)
import           Data.Hashable
import           GHC.Generics

newtype Number = Number Integer   deriving (Eq, Show)
newtype Label  = Label ByteString deriving (Eq, Show, Generic)

instance Hashable Label

data Instruction =
  -- Stack Manipulation
    Push Number
  | Dup
  | Swap
  | Drop
  -- Arithmetic
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  -- Heap Access
  | Store
  | Retrieve
  -- Flow Control
  | Mark Label
  | Func Label
  | Jump Label
  | JumpZero Label
  | JumpNeg  Label
  | Return
  | Exit
  -- IO
  | PrintChar
  | PrintNum
  | ReadChar
  | ReadNum
  deriving (Eq, Show)
