{-# Language NamedFieldPuns #-}
{-# Language Strict         #-}
{-# Language TypeFamilies   #-}
module Marginal.VM.Strict
  (
    run,
    step,
    start,
    VMStrict
  )
where

import           Data.Char
import           Data.HashMap.Strict
import qualified Data.IntMap.Strict as I
import qualified Data.Vector as V
import           System.IO (hFlush, stdout)

import           Marginal.Parse.Type
import           Marginal.VM.Type

type ProgramCounter = Int
type ReturnPointers = [ProgramCounter]
type VMHeap         = I.IntMap Integer
type VMStack        = [Integer]
type Labels         = HashMap Label ProgramCounter

data StepResult = VMExit
                | VMStart
                | VMOut String
                | VMErr String
                | VMGetChar Int
                | VMGetNum  Int
                | VMOther
                deriving (Eq, Show)

data VMStrict = VMStrict {
  pc         :: ProgramCounter,
  returnPtrs :: [ProgramCounter],
  stepResult :: StepResult,
  stack      :: VMStack,
  heap       :: VMHeap,
  labels     :: Labels
}

instance VM VMStrict where
  type VMOut VMStrict = IO
  run vm instructions = loop vm { labels = locateLabels instructions} instructions
    where
      loop vm instructions = do
        let instr = instructions V.! (pc vm)
        let vm'   = step vm instr
        case stepResult vm' of
          VMExit      -> pure vm'
          VMOut l     -> putStr l >> hFlush stdout >> loop vm' instructions
          VMErr e     -> vmErr instr (pc vm) e >> pure vm'
          VMGetChar l -> do
            c <- getChar
            let vm'' = vm'{
                  heap = I.insert l (fromIntegral . ord $ c) (heap vm'),
                  stepResult = VMOther
                  }
            loop vm'' instructions
          VMGetNum  l -> do
            input <- getLine
            let vm'' = vm'{
                  heap = I.insert l (read input) (heap vm'),
                  stepResult = VMOther
                  }
            loop vm'' instructions
          _        -> loop vm' instructions

  step  = step'
  start = strictStart

vmErr :: Instruction -> Int -> String -> IO ()
vmErr i loc e = do
  putStrLn $ "Error on instruction " <> show i <> " at " <> show loc
  putStrLn e

strictStart = VMStrict 0 [] VMStart [] I.empty empty

locateLabels :: V.Vector Instruction -> Labels
locateLabels = snd . V.foldl' acc (0, empty) where
  acc (i, m) (Mark l) = (i + 1, insert l (i + 1) m)
  acc (i, m) _        = (i + 1, m)

step' vm@(VMStrict pc rptrs _ stack heap labels) instr =
  let vm' = VMStrict (pc + 1) rptrs VMOther
      stackLen = show . length
      verr e = vm{ stepResult = VMErr e }
  in
  case instr of
    Push (Number n) -> vm' (n : stack) heap labels
    Dup             -> case stack of
                         (x : xs) -> vm' (x : x : xs) heap labels
                         []       -> verr "Stack empty, nothing to duplicate!"
    Swap             -> case stack of
                          (x : y : xs) -> vm' (y : x : xs) heap labels
                          _ -> verr $ "Stack too small to swap. Size: " ++ stackLen stack
    Drop             -> case stack of
                          (x : xs) -> vm' xs heap labels
                          _        -> verr "Stack empty, nothing to discard."
    Add              -> vm' (arithmetic stack Add) heap labels
    Sub              -> vm' (arithmetic stack Sub) heap labels
    Mult             -> vm' (arithmetic stack Mult) heap labels
    Div              -> vm' (arithmetic stack Div) heap labels
    Mod              -> vm' (arithmetic stack Mod) heap labels
    Store            -> case stack of
                          (val : key : xs) -> vm' stack (store val key heap) labels
                          _ -> verr $ "Stack too small to store. Size: " ++ stackLen stack
    Retrieve         -> case stack of
                          (key : xs) -> vm' (retrieve key heap : stack) heap labels
                          _ -> verr "Stack empty, unable to retrieve from heap"
    Mark label       -> vm' stack heap (insert label pc labels)
    Func label       -> call label vm
    Jump label       -> jump label vm
    JumpZero label   -> condJump (== 0) label vm
    JumpNeg label    -> condJump (<  0) label vm
    Return           -> case rptrs of
                          []           -> verr "Nothing to return to"
                          (r : rptrs') -> vm { pc = r, returnPtrs = rptrs'}
    Exit             -> vm { stepResult = VMExit }
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

call :: Label -> VMStrict -> VMStrict
call label (VMStrict pc rptrs _ stack heap labels) =
  VMStrict (labels ! label) ((pc + 1) : rptrs) VMOther stack heap labels

condJump :: (Integer -> Bool) -> Label -> VMStrict -> VMStrict
condJump _ label VMStrict{stack=[]} = error $ "Stack empty, unable to jump to :" ++ show label
condJump f label vm@(VMStrict pc rptrs _ stack@(x : xs) heap labels) =
  if f x
  then jump label vm
  else VMStrict (pc + 1) rptrs VMOther stack heap labels

jump :: Label -> VMStrict -> VMStrict
jump label vm@VMStrict{labels} = vm{ pc = labels ! label, stepResult = VMOther }

printVal instr vm@VMStrict{stack=[]} =
  vm{stepResult = VMErr $ "Stack empty, unable to " ++ show instr}
printVal PrintChar vm@VMStrict{stack} =
  incPC vm{stepResult = VMOut . pure . chr . fromIntegral . head $ stack}
printVal PrintNum vm@VMStrict{stack} =
  incPC vm{stepResult = VMOut . show . head $ stack}

readVal instr vm@VMStrict{ stack=[] } =
  vm{stepResult = VMErr $ "Stack empty, unable to " ++ show instr}
readVal ReadChar vm@VMStrict{ stack=(x : xs), heap } =
  incPC vm {stepResult = VMGetChar (fromIntegral x)}
readVal ReadNum vm@VMStrict { stack=(x:xs), heap } =
  incPC vm { stepResult = VMGetNum (fromIntegral x)}

incPC :: VMStrict -> VMStrict
incPC vm@VMStrict{pc} = vm { pc = pc + 1 }
