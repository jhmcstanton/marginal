{-# Language DataKinds    #-}
{-# Language TypeFamilies #-}
module Marginal.VM.Type where

import           Marginal.Parse.Type

import           Data.Proxy
import           Data.Vector (Vector)

-- Can't make implementations of VM for a data family
-- data family VMType :: *
data VMType = Strict | StrictDebug deriving (Read, Show)

class VM (v :: VMType) where
  data VMState v :: *
  type VMOut v   :: * -> *
  run            :: (VMState v)
                 -> Vector Instruction
                 -> VMOut v (VMState v)
  step           :: (VMState v)
                 -> Instruction
                 -> (VMState v)
  start          :: VMState v
