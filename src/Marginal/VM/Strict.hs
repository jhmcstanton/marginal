{-# Language DataKinds    #-}
{-# Language Strict       #-}
{-# Language TypeFamilies #-}
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
import           System.Exit (exitSuccess)

import           Marginal.Parse.Type
import           Marginal.VM.Type

type ProgramCounter = Int
type ReturnPointers = [ProgramCounter]
type VMHeap         = I.IntMap Integer
type VMStack        = [Integer]
type Labels         = HashMap Label ProgramCounter

instance VMType Strict where
  data VM Strict = VMStrict {
    pc         :: ProgramCounter,
    returnPtrs :: [ProgramCounter],
    stack      :: VMStack,
    heap       :: VMHeap,
    labels     :: Labels
  }
  type VMOut Strict = IO
  run vm instructions = loop vm { labels = locateLabels instructions} instructions
    where
      loop vm instructions = do
        let instr = instructions V.! (pc vm)
        putStrLn (show instr) -- TODO: remove this
        (vm', ()) <- step vm instr
        run vm' instructions
      -- instrs' = V.filter notMark instructions
      -- notMark (Mark _) = False
      -- notMark _        = True

  step = step'

start = VMStrict 0 [] [] I.empty empty

locateLabels :: V.Vector Instruction -> Labels
locateLabels = snd . V.foldr' acc (0, empty) where
  acc (Mark l) (i, m) = (i + 1, insert l (i + 1) m)
  acc _ (i, m)        = (i + 1, m)

step' vm@(VMStrict pc rptrs stack heap labels) instr =
  let noIO s = pure (s, ())
      vm' = VMStrict (pc + 1) rptrs
      stackLen = show . length
  in
  case instr of
    Push (Number n) -> noIO $ vm' (n : stack) heap labels
    Dup             -> case stack of
                         (x : xs) -> noIO $ vm' (x : x : xs) heap labels
                         []       -> error "Stack empty, nothing to duplicate!"
    Swap             -> case stack of
                          (x : y : xs) -> noIO $ vm' (y : x : xs) heap labels
                          _ -> error $ "Stack too small to swap. Size: " ++ stackLen stack
    Drop             -> case stack of
                          (x : xs) -> noIO $ vm' xs heap labels
                          _        -> error "Stack empty, nothing to discard."
    Add              -> noIO $ vm' (arithmetic stack Add) heap labels
    Sub              -> noIO $ vm' (arithmetic stack Sub) heap labels
    Mult             -> noIO $ vm' (arithmetic stack Mult) heap labels
    Div              -> noIO $ vm' (arithmetic stack Div) heap labels
    Mod              -> noIO $ vm' (arithmetic stack Mod) heap labels
    Store            -> case stack of
                          (val : key : xs) -> noIO $ vm' stack (store val key heap) labels
                          _ -> error $ "Stack too small to store. Size: " ++ stackLen stack
    Retrieve         -> case stack of
                          (key : xs) -> noIO $ vm' (retrieve key heap : stack) heap labels
                          _ -> error "Stack empty, unable to retrieve from heap"
    Mark label       -> noIO $ vm' stack heap (insert label pc labels)
    Func label       -> noIO $ call label vm
    Jump label       -> noIO $ jump (const True) label vm
    JumpZero label   -> noIO $ jump (== 0) label vm
    JumpNeg label    -> noIO $ jump (<  0) label vm
    Return           -> case rptrs of
                          []        -> error "Nothing to return to"
                          (r : rptrs') ->
                            noIO $ vm { pc = r, returnPtrs = rptrs'}
    Exit             -> exitSuccess >> pure (start, ())
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
call label (VMStrict pc rptrs stack heap labels) =
  VMStrict (labels ! label) ((pc + 1) : rptrs) stack heap labels

jump :: (Integer -> Bool) -> Label -> VM Strict -> VM Strict
jump _ label (VMStrict _ _ [] _ _) = error $ "Stack empty, unable to jump to :" ++ show label
jump f label (VMStrict pc rptrs stack@(x : xs) heap labels) =
  if f x
  then VMStrict (labels ! label) rptrs stack heap labels
  else VMStrict (pc + 1) rptrs stack heap labels

printVal instr (VMStrict _ _ [] _ _) =
  error $ "Stack empty, unable to " ++ show instr
printVal PrintChar vm@(VMStrict _ _ stack _ _) = do
  putChar . chr . fromIntegral . head $ stack
  pure (incPC vm, ())
printVal PrintNum vm@(VMStrict _ _ stack _ _)  = do
  putStrLn . show . head $ stack
  pure (incPC vm, ())

readVal instr (VMStrict _ _ [] _ _) =
  error $ "Stack empty, unable to " ++ show instr
readVal ReadChar vm@(VMStrict _ _ (x : xs) heap _) =
  getChar >>= \input -> pure (incPC vm {
                          heap = I.insert (fromIntegral x) (fromIntegral . ord $ input) heap
                          }, ())
readval ReadNum vm@(VMStrict _ _ (x : xs) heap _) =
  getLine >>= \input -> pure (incPC vm {
                              heap = I.insert (fromIntegral x) (read input) heap
                             }, ())

incPC :: VM Strict -> VM Strict
incPC vm@(VMStrict pc _ _ _ _) = vm { pc = pc + 1 }
