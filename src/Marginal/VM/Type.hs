{-# Language TypeFamilies #-}
module Marginal.VM.Type where

import           Marginal.Parse.Type

import           Data.Proxy
import           Data.Vector (Vector)

class VM v where
  type VMOut v :: * -> *
  run          :: v
               -> Vector Instruction
               -> VMOut v v
  step         :: v
               -> Instruction
               -> v
  start        :: v
