module Asmbunny where

import Control.DeepSeq

data Argument = Number Int | Register Int deriving (Eq, Show)
data Instr =
    Copy Argument Argument |
    Incr Argument |
    Decr Argument |
    JumpNZ Argument Argument |
    Toggle Argument deriving (Eq, Show)

-- instructions, position, registers
type VM = ([Instr], Int, [Int])

readArg :: String -> Argument
readArg "a" = Register 0
readArg "b" = Register 1
readArg "c" = Register 2
readArg "d" = Register 3
readArg a = Number $ read a

readInstr :: [String] -> [Instr]
readInstr [] = []
readInstr ("cpy":a:b:xs) = Copy (readArg a) (readArg b) : readInstr xs
readInstr ("inc":a:xs) = Incr (readArg a) : readInstr xs
readInstr ("dec":a:xs) = Decr (readArg a) : readInstr xs
readInstr ("jnz":a:b:xs) = JumpNZ (readArg a) (readArg b) : readInstr xs
readInstr ("tgl":a:xs) = Toggle (readArg a) : readInstr xs

newVM :: [Instr] -> [Int] -> VM
newVM instr regs = (instr,0,regs)

get :: [Int] -> Argument -> Int
get _ (Number n) = n
get regs (Register id) = regs !! id

put :: [Int] -> Argument -> Int -> [Int]
put regs (Register pos) val = take pos regs ++ [val] ++ drop (pos + 1) regs

jnz :: Int -> Int -> Int -> Int
jnz num pc offset
  | num == 0 = pc + 1
  | otherwise = pc + offset

toggle :: [Instr] -> Int -> [Instr]
toggle instr pos
 | pos < 0 || pos >= length instr = instr
 | otherwise = (take pos instr) ++ [toggleInstr (instr !! pos)] ++ drop (pos+1) instr

toggleInstr :: Instr -> Instr
toggleInstr (Incr a) = (Decr a)
toggleInstr (Decr a) = (Incr a)
toggleInstr (Toggle a) = (Incr a)
toggleInstr (Copy a b) = (JumpNZ a b)
toggleInstr (JumpNZ a b) = (Copy a b)

stepVM :: VM -> Instr -> VM
stepVM (code,pc,reg) (Copy from to) = (code,pc+1,put reg to (get reg from))
stepVM (code,pc,reg) (Incr a) = (code,pc+1,put reg a (1 + get reg a))
stepVM (code,pc,reg) (Decr a) = (code,pc+1,put reg a (-1 + get reg a))
stepVM (code,pc,reg) (JumpNZ a b) = (code,jnz (get reg a) pc (get reg b),reg)
stepVM (code,pc,reg) (Toggle a) = (toggle code (pc+get reg a), pc+1, reg)

runVM :: VM -> VM
runVM vm@(instr,pc,_)
  | pc < 0 = vm
  | pc >= length instr = vm
  | otherwise = runVM $ stepVM vm (instr !! pc)

