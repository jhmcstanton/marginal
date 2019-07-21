{-# Language OverloadedStrings #-}

import Data.Char
import Data.Vector (fromList)

import Marginal.Parse.Type

toBin 0 = [0]
toBin n = reverse (helper n) where
  helper 0 = []
  helper n = (n `mod` 2) : helper (n `div` 2)

toWChar 1 = '1'
toWChar 0 = '0'

toWhiteSpace xs = "   " ++ fmap toWChar xs ++ "\n"

charToPush = toWhiteSpace . toBin . ord

stringToPushes = map charToPush . reverse

collatzMain = fromList [
     Func (Label "num_prompt"),
     Push $ counter,
     Push $ Number (-1),
     Store,
     Drop,
     Drop,
     Func (Label "collatz"),
     Push counter,
     Retrieve,
     PrintNum,
     Push $ Number 10,
     PrintChar,
     Exit,
     Mark (Label "collatz"),
       Func (Label "increment_counter"),
       Dup,
       Push (Number 1),
       Sub,
       JumpNeg (Label "collatz_no_return"),
       Return,
       Mark (Label "collatz_no_return"),
         Drop,
         Dup,
         Push (Number 2),
         Swap,
         Mod,
         JumpZero (Label "collatz_even"),
         Drop,
         Push (Number 3),
         Mult,
         Push (Number 1),
         Add,
         Jump (Label "collatz"),
       Mark (Label "collatz_even"),
         Drop,
         Push (Number 2),
         Swap,
         Div,
         Jump (Label "collatz"),
     Mark (Label "num_prompt"),
       Push (Number 32),
       Push (Number 58),
       Push (Number 114),
       Push (Number 101),
       Push (Number 98),
       Push (Number 109),
       Push (Number 117),
       Push (Number 78),
       Dup,
       Mark (Label "_num_prompt_inner_"),
         Drop,
         PrintChar,
         Push (Number 32),
         Sub,
         JumpNeg (Label "_num_prompt_inner_"),
       Drop,
       Push start,
       ReadNum,
       Retrieve,
       Swap,
       Drop,
       Return,
     Mark (Label "increment_counter"),
       Push counter,
       Retrieve,
       Push (Number 1),
       Add,
       Store,
       Drop,
       Drop,
       Return
     ] where
     counter = Number 0
     start   = Number 1
