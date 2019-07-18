{-# Language DataKinds    #-}
{-# Language TypeFamilies #-}
module Marginal.VM.Type where

import           Marginal.Parse.Type

import           Data.Vector (Vector)

data EvalStrategy = Lazy | Strict deriving (Eq, Read, Show)

class VMType (strategy :: EvalStrategy) where
  data VM strategy    :: *
  type VMOut strategy :: * -> *
  run                 :: VM strategy
                      -> Vector Instruction
                      -> VMOut strategy (VM strategy)
  step                :: VM strategy
                      -> Instruction
                      -> VMOut strategy (VM strategy)
  start               :: VM strategy
