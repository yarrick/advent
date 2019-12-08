
parse :: String -> [Int]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

step :: (Int,[Int],Int,String) -> (Int,[Int],Int,String)
step d@(pc,mem,_,_)
  | op == 1 = compute (+) d immediate
  | op == 2 = compute (*) d immediate
  | op == 3 = input d
  | op == 4 = output d immediate
  | op == 5 = jump (0/=) d immediate
  | op == 6 = jump (0==) d immediate
  | op == 7 = cmp (<) d immediate
  | op == 8 = cmp (==) d immediate
    where
      reg = mem !! pc
      op = mod reg 100
      immediate = opflags reg

-- Always starts with a False value to keep 1-based indexing
opflags :: Int -> [Bool]
opflags reg = False : (take 3 $ map ('1'==) $ drop 2 $ reverse $ ("000" ++ show reg))

memset :: (Int,[Int],Int,String) -> Int -> Int -> Int -> (Int,[Int],Int,String)
memset (pc,mem,inp,out) newpc pos value = (newpc, take pos mem ++ [value] ++ drop (pos + 1) mem, inp, out)

compute :: (Int -> Int -> Int) -> (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
compute op d@(pc,mem,inp,out) imm = memset d (pc+4) res (op a b)
    where
      a = fetch pc mem imm 1
      b = fetch pc mem imm 2
      res = mem !! (pc + 3)

-- immediate or position access
fetch pc mem imm arg
  | imm !! arg = reg
  | otherwise = mem !! reg
    where reg = mem !! (pc + arg)

input :: (Int,[Int],Int,String) -> (Int,[Int],Int,String)
input d@(pc,mem,inp,out) = memset d (pc+2) pos inp
  where pos = mem !! (pc + 1)

output :: (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
output (pc,mem,inp,out) imm = (pc+2, mem, inp, out ++ show val ++ " ")
  where val = fetch pc mem imm 1

jump :: (Int -> Bool) -> (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
jump op (pc,mem,inp,out) imm
  | op arg = (pos, mem, inp, out)
  | otherwise = (pc+3, mem, inp, out)
    where
      arg = fetch pc mem imm 1
      pos = fetch pc mem imm 2

cmp :: (Int -> Int -> Bool) -> (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
cmp op d@(pc,mem,inp,out) imm
  | op a b = memset d (pc+4) res 1
  | otherwise = memset d (pc+4) res 0
    where
      a = fetch pc mem imm 1
      b = fetch pc mem imm 2
      res = mem !! (pc + 3)

run :: String -> String
run bytes = out
  where (pc,mem,inp,out) = exec (0, parse bytes, 1, [])

exec :: (Int,[Int],Int,String) -> (Int,[Int],Int,String)
exec d@(pc,mem,inp,out)
  | (mem !! pc) == 99 = (pc,mem,inp,out++"END")
  | otherwise = exec $ step d

-- part 2

test :: String -> Int -> String
test bytes input = show $ exec (0, parse bytes, input, [])

run2 :: String -> String
run2 bytes = out
  where (pc,mem,inp,out) = exec (0, parse bytes, 5, [])

