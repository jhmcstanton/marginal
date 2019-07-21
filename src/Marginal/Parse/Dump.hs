{-# Language OverloadedStrings #-}
module Marginal.Parse.Dump (dumpOPs) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char (ord)
import           Data.Foldable (foldMap)

import           Marginal.Parse.Type

dumpOPs :: Foldable f => f Instruction -> B.ByteString
dumpOPs = foldMap toWSOP

toWSNum :: Number -> B.ByteString
toWSNum (Number n) = foldMap (B.singleton . toChar) nums where
  nums   = sign : reverse (loop (abs n))
  sign   = if n >= 0 then 0 else 1
  toChar 0 = ' '
  toChar 1 = '\t'
  loop 0 = []
  loop n = n `mod` 2 : loop (n `div` 2)

numBytes :: Number -> B.ByteString
numBytes (Number n) = B.pack . show $ n

wsLabel :: Label -> B.ByteString
wsLabel (Label l) = foldMap (toWSNum . Number . fromIntegral . ord) l' where
  l' = B.unpack l

toWSOP :: Instruction -> B.ByteString
toWSOP (Push n)     = "--Push_" <> numBytes n <> "  " <> toWSNum n <> "\nend--"
toWSOP  Dup         = "--Dup \n end--"
toWSOP  Swap        = "--Swap \n\tend--"
toWSOP  Drop        = "--Drop \n\nend--"
toWSOP  Add         = "--Add\t   end--"
toWSOP  Sub         = "--Sub\t  \tend--"
toWSOP  Mult        = "--Mult\t  \nend--"
toWSOP  Div         = "--Div\t \t end--"
toWSOP  Mod         = "--Mod\t \t\tend--"
toWSOP  Store       = "--Store\t\t end--"
toWSOP  Retrieve    = "--Retrieve\t\t\tend--"
toWSOP (Mark     l) = "--Mark\n  "      <> wsLabel l <> "\nend--"
toWSOP (Func     l) = "--Func\n \t"     <> wsLabel l <> "\nend--"
toWSOP (Jump     l) = "--Jump\n \n"     <> wsLabel l <> "\nend--"
toWSOP (JumpZero l) = "--JumpZero\n\t " <> wsLabel l <> "\nend--"
toWSOP (JumpNeg  l) = "--JumpNeg\n\t\t" <> wsLabel l <> "\nend--"
toWSOP  Return      = "--Return\n\t\nend--"
toWSOP  Exit        = "--Exit\n\n\nend--"
toWSOP  PrintChar   = "--PrintChar\t\n  end--"
toWSOP  PrintNum    = "--PrintNum\t\n \tend--"
toWSOP  ReadChar    = "--ReadChar\t\n\t end--"
toWSOP  ReadNum     = "--ReadNum\t\n\t\tend--"
