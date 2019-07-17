import Data.Char

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
