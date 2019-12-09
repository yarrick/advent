import Data.List

type State = (
  Integer,   -- pc
  [Integer], -- memory
  [Integer], -- inputs
  [Integer], -- output
  Integer)   -- relative base

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

getmem :: [Integer] -> Integer -> Integer
getmem mem p = head $ drop (fromInteger p) mem

step :: State -> State
step d@(pc,mem,_,_,_)
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
opflags reg = 0 : (take 3 $ map read $ map (\c -> [c]) $ drop 2 $ reverse $ ("000" ++ show reg))

memset :: State -> Integer -> Integer -> Integer -> State
memset (pc,mem,inp,out,base) newpc pos value =
  (newpc, take (fromInteger pos) mem ++ [value] ++ drop (fromInteger (pos + 1)) mem, inp, out, base)

compute :: (Integer -> Integer -> Integer) -> State -> [Integer] -> State
compute op d@(pc,mem,inp,out,_) imm = memset d (pc+4) res (op a b)
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

-- immediate or position access
fetch :: State -> [Integer] -> Integer -> Integer
fetch (pc,mem,_,_,base) imm offset
  | getmem imm offset == 1 = reg
  | getmem imm offset == 2 = getmem mem (base + reg)
  | otherwise = getmem mem reg
    where reg = getmem mem (pc + offset)

outpos :: State -> [Integer] -> Integer -> Integer
outpos d@(pc,mem,_,_,base) imm offset
  | getmem imm offset == 2 = base + reg
  | otherwise = reg
    where reg = getmem mem (pc + offset)

input :: State -> [Integer] -> State
input d@(pc,mem,inp,out,base) imm = memset (pc,mem,tail inp,out,base) (pc+2) pos $ head inp
  where pos = outpos d imm 1

output :: State -> [Integer] -> State
output d@(pc,mem,inp,out,base) imm = (pc+2, mem, inp, out ++ [val], base)
  where val = fetch d imm 1

jump :: (Integer -> Bool) -> State -> [Integer] -> State
jump op d@(pc,mem,inp,out,base) imm
  | op arg = (pos, mem, inp, out, base)
  | otherwise = (pc+3, mem, inp, out, base)
    where
      arg = fetch d imm 1
      pos = fetch d imm 2

cmp :: (Integer -> Integer -> Bool) -> State -> [Integer] -> State
cmp op d@(pc,mem,inp,out,_) imm
  | op a b = memset d (pc+4) res 1
  | otherwise = memset d (pc+4) res 0
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

relbase :: State -> [Integer] -> State
relbase d@(pc,mem,inp,out,base) imm = (pc+2,mem,inp,out, base + fetch d imm 1)

exec :: State -> State
exec d@(pc,mem,_,_,_)
  | (getmem mem pc) == 99 = d
  | otherwise = exec $ step d

newstate :: [Integer] -> [Integer] -> State
newstate instr input = (0, instr ++ (take 20000 $ repeat 0), input, [], 0)

test :: String -> [Integer] -> ([Integer],[Integer])
test bytes input = (take 100 mem, out)
  where (_,mem,_,out,_) = exec $ newstate (parse bytes) input

run :: String -> [Integer] -> [Integer]
run bytes input = out
  where (_,_,_,out,_) = exec $ newstate (parse bytes) input
