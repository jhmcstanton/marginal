{-# Language DataKinds      #-}
{-# Language NamedFieldPuns #-}
{-# Language Strict         #-}
{-# Language TypeFamilies   #-}
module Marginal.VM.Strict
  (
    run,
    step,
    start
  )
where

import           Data.Char
import           Data.HashMap.Strict
import qualified Data.IntMap.Strict as I
import qualified Data.Vector as V

import           Marginal.Parse.Type
import           Marginal.VM.Type

type ProgramCounter = Int
type ReturnPointers = [ProgramCounter]
type VMHeap         = I.IntMap Integer
type VMStack        = [Integer]
type Labels         = HashMap Label ProgramCounter

data StepResult = VMExit | VMStart | VMOther deriving (Eq, Show)

instance VMType Strict where
  data VM Strict = VMStrict {
    pc         :: ProgramCounter,
    returnPtrs :: [ProgramCounter],
    stepResult :: StepResult,
    stack      :: VMStack,
    heap       :: VMHeap,
    labels     :: Labels
  }
  type VMOut Strict   = IO
  run vm instructions = loop vm { labels = locateLabels instructions} instructions
    where
      loop vm instructions = do
        let instr = instructions V.! (pc vm)
        vm' <- step vm instr
        case stepResult vm' of
          VMExit -> pure vm'
          _      -> run vm' instructions

  step = step'

start = VMStrict 0 [] VMStart [] I.empty empty

locateLabels :: V.Vector Instruction -> Labels
locateLabels = snd . V.foldl' acc (0, empty) where
  acc (i, m) (Mark l) = (i + 1, insert l (i + 1) m)
  acc (i, m) _        = (i + 1, m)

step' vm@(VMStrict pc rptrs _ stack heap labels) instr =
  let vm' = VMStrict (pc + 1) rptrs VMOther
      stackLen = show . length
  in
  case instr of
    Push (Number n) -> pure $ vm' (n : stack) heap labels
    Dup             -> case stack of
                         (x : xs) -> pure $ vm' (x : x : xs) heap labels
                         []       -> error "Stack empty, nothing to duplicate!"
    Swap             -> case stack of
                          (x : y : xs) -> pure $ vm' (y : x : xs) heap labels
                          _ -> error $ "Stack too small to swap. Size: " ++ stackLen stack
    Drop             -> case stack of
                          (x : xs) -> pure $ vm' xs heap labels
                          _        -> error "Stack empty, nothing to discard."
    Add              -> pure $ vm' (arithmetic stack Add) heap labels
    Sub              -> pure $ vm' (arithmetic stack Sub) heap labels
    Mult             -> pure $ vm' (arithmetic stack Mult) heap labels
    Div              -> pure $ vm' (arithmetic stack Div) heap labels
    Mod              -> pure $ vm' (arithmetic stack Mod) heap labels
    Store            -> case stack of
                          (val : key : xs) -> pure $ vm' stack (store val key heap) labels
                          _ -> error $ "Stack too small to store. Size: " ++ stackLen stack
    Retrieve         -> case stack of
                          (key : xs) -> pure $ vm' (retrieve key heap : stack) heap labels
                          _ -> error "Stack empty, unable to retrieve from heap"
    Mark label       -> pure $ vm' stack heap (insert label pc labels)
    Func label       -> pure $ call label vm
    Jump label       -> pure $ jump label vm
    JumpZero label   -> pure $ condJump (== 0) label vm
    JumpNeg label    -> pure $ condJump (<  0) label vm
    Return           -> case rptrs of
                          []        -> error "Nothing to return to"
                          (r : rptrs') ->
                            pure $ vm { pc = r, returnPtrs = rptrs'}
    Exit             -> pure vm { stepResult = VMExit }
    PrintChar        -> printVal PrintChar vm
    PrintNum         -> printVal PrintNum vm
    ReadChar         -> readVal ReadChar vm
    ReadNum          -> readVal ReadNum vm

arithmetic (x : y : xs) instr =
  case instr of
    Add  -> (x + y)   : xs
    Sub  -> (x - y)   : xs
    Mult -> (x * y)   : xs
    Div  -> (x `div` y)   : xs
    Mod  -> x `mod` y : xs
arithmetic _ instr = error $ "Not enough operands for " ++ show instr

store :: Integer -> Integer -> I.IntMap Integer -> I.IntMap Integer
store val key map = I.insert (fromIntegral key) val map

retrieve :: Integer -> I.IntMap Integer -> Integer
retrieve val map = map I.! (fromIntegral val)

call :: Label -> VM Strict -> VM Strict
call label (VMStrict pc rptrs _ stack heap labels) =
  VMStrict (labels ! label) ((pc + 1) : rptrs) VMOther stack heap labels

condJump :: (Integer -> Bool) -> Label -> VM Strict -> VM Strict
condJump _ label VMStrict{stack=[]} = error $ "Stack empty, unable to jump to :" ++ show label
condJump f label vm@(VMStrict pc rptrs _ stack@(x : xs) heap labels) =
  if f x
  then jump label vm
  else VMStrict (pc + 1) rptrs VMOther stack heap labels

jump :: Label -> VM Strict -> VM Strict
jump label vm@VMStrict{labels} = vm{ pc = labels ! label, stepResult = VMOther }

printVal instr VMStrict{stack=[]} =
  error $ "Stack empty, unable to " ++ show instr
printVal PrintChar vm@VMStrict{stack} = do
  putChar . chr . fromIntegral . head $ stack
  pure (incPC vm)
printVal PrintNum vm@VMStrict{stack}  = do
  putStr . show . head $ stack
  pure (incPC vm)

readVal instr VMStrict{ stack=[] } =
  error $ "Stack empty, unable to " ++ show instr
readVal ReadChar vm@VMStrict{ stack=(x : xs), heap } =
  getChar >>= \input ->
    pure (incPC vm {
           heap = I.insert (fromIntegral x) (fromIntegral . ord $ input) heap
         })
readval ReadNum vm@VMStrict { stack=(x:xs), heap } =
  getLine >>= \input ->
    pure (incPC vm {
           heap = I.insert (fromIntegral x) (read input) heap
         })

incPC :: VM Strict -> VM Strict
incPC vm@VMStrict{pc} = vm { pc = pc + 1 }
