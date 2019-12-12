module Intcode where

data State = State {
  pc :: Integer,
  memory :: [Integer],
  indata :: [Integer],
  outdata ::[Integer],
  relbase :: Integer,
  haltfunc :: [Integer] -> Bool}

instance Show (State) where
  show st = "PC " ++ (show $ pc st) ++ " Mem " ++ show (take 32 $ memory st)
    ++ " Input " ++ show (indata st) ++ " Output " ++ show (outdata st)
    ++ " Relbase " ++ show (relbase st)

getmem :: [Integer] -> Integer -> Integer
getmem mem p = head $ drop (fromInteger p) mem

step :: State -> State
step st
  | op == 1 = compute (+) st immediate
  | op == 2 = compute (*) st immediate
  | op == 3 = input st immediate
  | op == 4 = output st immediate
  | op == 5 = jump (0/=) st immediate
  | op == 6 = jump (0==) st immediate
  | op == 7 = cmp (<) st immediate
  | op == 8 = cmp (==) st immediate
  | op == 9 = movrelbase st immediate
    where
      reg = getmem (memory st) (pc st)
      op = mod reg 100
      immediate = opflags reg

-- Always starts with a zero value to keep 1-based indexing
opflags :: Integer -> [Integer]
opflags reg = 0 : (take 3 $ map read $ map (\c -> [c]) $ drop 2 $ reverse $ ("0000" ++ show reg))

memset :: State -> Integer -> Integer -> Integer -> State
memset st newpc pos value =
  st { pc = newpc,
       memory = take (fromInteger pos) (memory st) ++ [value] ++ drop (fromInteger (pos + 1)) (memory st) }

compute :: (Integer -> Integer -> Integer) -> State -> [Integer] -> State
compute op st imm = memset st ((pc st)+4) res (op a b)
    where
      a = fetch st imm 1
      b = fetch st imm 2
      res = outpos st imm 3

-- immediate or position access
fetch :: State -> [Integer] -> Integer -> Integer
fetch st imm offset
  | getmem imm offset == 1 = reg
  | getmem imm offset == 2 = getmem (memory st) ((relbase st) + reg)
  | otherwise = getmem (memory st) reg
    where reg = getmem (memory st) ((pc st) + offset)

outpos :: State -> [Integer] -> Integer -> Integer
outpos st imm offset
  | getmem imm offset == 2 = (relbase st) + reg
  | otherwise = reg
    where reg = getmem (memory st) ((pc st) + offset)

input :: State -> [Integer] -> State
input st imm = memset (st { indata = tail (indata st) }) ((pc st)+2) pos $ head (indata st)
  where pos = outpos st imm 1

output :: State -> [Integer] -> State
output st imm = st { pc = (pc st) +2, outdata = (outdata st) ++ [val] }
  where val = fetch st imm 1

jump :: (Integer -> Bool) -> State -> [Integer] -> State
jump op st imm
  | op arg = st { pc = pos }
  | otherwise = st { pc = (pc st) + 3 }
    where
      arg = fetch st imm 1
      pos = fetch st imm 2

cmp :: (Integer -> Integer -> Bool) -> State -> [Integer] -> State
cmp op st imm
  | op a b = memset st ((pc st)+4) res 1
  | otherwise = memset st ((pc st)+4) res 0
    where
      a = fetch st imm 1
      b = fetch st imm 2
      res = outpos st imm 3

movrelbase :: State -> [Integer] -> State
movrelbase st imm = st { pc = (pc st) + 2, relbase = (relbase st) + fetch st imm 1 }

exec :: State -> State
exec st
  | (haltfunc st) (outdata st) = st
  | (getmem (memory st) (pc st)) == 99 = st { pc = -1 }
  | otherwise = exec $ step st

newstate :: [Integer] -> [Integer] -> State
newstate instr input = newhaltstate instr input (\x -> False)

newhaltstate :: [Integer] -> [Integer] -> ([Integer] -> Bool) -> State
newhaltstate instr input halter = State { pc = 0, memory = instr ++ (take 1000 $ repeat 0),
  indata = input, outdata = [], relbase = 0, haltfunc = halter }

inputnum :: State -> Integer -> State
inputnum st num = st { indata = (indata st) ++ [num] }

