
parse :: String -> [Int]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

step :: (Int,[Int],Int,String) -> (Int,[Int],Int,String)
step d@(pc,mem,inp,out)
  | op == 1 = compute (+) d immediate
  | op == 2 = compute (*) d immediate
  | op == 3 = input d
  | op == 4 = output d immediate
  | otherwise = (0,[99],inp,out++"ERR " ++ show op)
    where
      reg = mem !! pc
      op = mod reg 100
      immediate = opflags reg

-- Always starts with a False value to keep 1-based indexing
opflags :: Int -> [Bool]
opflags reg = False : (take 3 $ map ('1'==) $ drop 2 $ reverse $ ("000" ++ show reg))

compute :: (Int -> Int -> Int) -> (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
compute op (pc,mem,inp,out) imm = (pc+4, take res mem ++ [op a b] ++ drop (res + 1) mem, inp, out)
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
input (pc,mem,inp,out) = (pc+2, take pos mem ++ [inp] ++ drop (pos + 1) mem, inp, out)
    where pos = mem !! (pc + 1)

output :: (Int,[Int],Int,String) -> [Bool] -> (Int,[Int],Int,String)
output (pc,mem,inp,out) imm = (pc+2, mem, inp, out ++ show val ++ " ")
    where val = fetch pc mem imm 1

run :: String -> String
run bytes = out
  where (pc,mem,inp,out) = exec (0, parse bytes, 1, [])

exec :: (Int,[Int],Int,String) -> (Int,[Int],Int,String)
exec d@(pc,mem,inp,out)
  | (mem !! pc) == 99 = (pc,mem,inp,out++"END")
  | otherwise = exec $ step d
