module Intcode where

type State = (
  Integer,   -- pc
  [Integer], -- memory
  [Integer], -- inputs
  [Integer], -- output
  Integer,   -- relative base
  [Integer] -> Bool) -- halt func

getmem :: [Integer] -> Integer -> Integer
getmem mem p = head $ drop (fromInteger p) mem

step :: State -> State
step d@(pc,mem,_,_,_,_)
  | op == 1 = compute (+) d immediate
  | op == 2 = compute (*) d immediate
  | op == 3 = input d immediate
  | op == 4 = output d immediate
  | op == 5 = jump (0/=) d immediate
  | op == 6 = jump (0==) d immediate
  | op == 7 = cmp (<) d immediate
  | op == 8 = cmp (==) d immediate
  | op == 9 = relbase d immediate
    where
      reg = getmem mem pc
      op = mod reg 100
      immediate = opflags reg

-- Always starts with a zero value to keep 1-based indexing
opflags :: Integer -> [Integer]
opflags reg = 0 : (take 3 $ map read $ map (\c -> [c]) $ drop 2 $ reverse $ ("0000" ++ show reg))

memset :: State -> Integer -> Integer -> Integer -> State
memset (pc,mem,inp,out,base,h) newpc pos value =
  (newpc, take (fromInteger pos) mem ++ [value] ++ drop (fromInteger (pos + 1)) mem, inp, out, base, h)

compute :: (Integer -> Integer -> Integer) -> State -> [Integer] -> State
compute op d@(pc,mem,inp,out,_,_) imm = memset d (pc+4) res (op a b)
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

-- immediate or position access
fetch :: State -> [Integer] -> Integer -> Integer
fetch (pc,mem,_,_,base,_) imm offset
  | getmem imm offset == 1 = reg
  | getmem imm offset == 2 = getmem mem (base + reg)
  | otherwise = getmem mem reg
    where reg = getmem mem (pc + offset)

outpos :: State -> [Integer] -> Integer -> Integer
outpos d@(pc,mem,_,_,base,_) imm offset
  | getmem imm offset == 2 = base + reg
  | otherwise = reg
    where reg = getmem mem (pc + offset)

input :: State -> [Integer] -> State
input d@(pc,mem,inp,out,base,h) imm = memset (pc,mem,tail inp,out,base,h) (pc+2) pos $ head inp
  where pos = outpos d imm 1

output :: State -> [Integer] -> State
output d@(pc,mem,inp,out,base,h) imm = (pc+2, mem, inp, out ++ [val], base, h)
  where val = fetch d imm 1

jump :: (Integer -> Bool) -> State -> [Integer] -> State
jump op d@(pc,mem,inp,out,base,h) imm
  | op arg = (pos, mem, inp, out, base, h)
  | otherwise = (pc+3, mem, inp, out, base, h)
    where
      arg = fetch d imm 1
      pos = fetch d imm 2

cmp :: (Integer -> Integer -> Bool) -> State -> [Integer] -> State
cmp op d@(pc,mem,inp,out,_,_) imm
  | op a b = memset d (pc+4) res 1
  | otherwise = memset d (pc+4) res 0
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

relbase :: State -> [Integer] -> State
relbase d@(pc,mem,inp,out,base,h) imm = (pc+2,mem,inp,out, base + fetch d imm 1,h)

exec :: State -> State
exec d@(pc,mem,_,out,_,halter)
  | halter out = d
  | (getmem mem pc) == 99 = d
  | otherwise = exec $ step d

newstate :: [Integer] -> [Integer] -> State
newstate instr input = (0, instr ++ (take 20000 $ repeat 0), input, [], 0, (\x -> False))

newhaltstate :: [Integer] -> [Integer] -> ([Integer] -> Bool) -> State
newhaltstate instr input halter = (0, instr ++ (take 20000 $ repeat 0), input, [], 0, halter)

inputnum :: State -> Integer -> State
inputnum (pc,mem,inp,out,base,h) num = (pc,mem,inp ++ [num],out,base,h)

