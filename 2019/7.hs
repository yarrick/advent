
parse :: String -> [Int]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

step :: (Int,[Int],[Int],String) -> (Int,[Int],[Int],String)
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

memset :: (Int,[Int],[Int],String) -> Int -> Int -> Int -> (Int,[Int],[Int],String)
memset (pc,mem,inp,out) newpc pos value = (newpc, take pos mem ++ [value] ++ drop (pos + 1) mem, inp, out)

compute :: (Int -> Int -> Int) -> (Int,[Int],[Int],String) -> [Bool] -> (Int,[Int],[Int],String)
compute op d@(pc,mem,inp,out) imm = memset d (pc+4) res (op a b)
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = mem !! (pc + 3)

-- immediate or position access
fetch :: (Int,[Int],[Int],String) -> [Bool] -> Int -> Int
fetch (pc,mem,_,_) imm offset
  | imm !! offset = reg
  | otherwise = mem !! reg
    where reg = mem !! (pc + offset)

input :: (Int,[Int],[Int],String) -> (Int,[Int],[Int],String)
input (pc,mem,inp,out) = memset (pc,mem,tail inp,out) (pc+2) pos $ head inp
  where pos = mem !! (pc + 1)

output :: (Int,[Int],[Int],String) -> [Bool] -> (Int,[Int],[Int],String)
output d@(pc,mem,inp,out) imm = (pc+2, mem, inp, out ++ show val ++ " ")
  where val = fetch d imm 1

jump :: (Int -> Bool) -> (Int,[Int],[Int],String) -> [Bool] -> (Int,[Int],[Int],String)
jump op d@(pc,mem,inp,out) imm
  | op arg = (pos, mem, inp, out)
  | otherwise = (pc+3, mem, inp, out)
    where
      arg = fetch d imm 1
      pos = fetch d imm 2

cmp :: (Int -> Int -> Bool) -> (Int,[Int],[Int],String) -> [Bool] -> (Int,[Int],[Int],String)
cmp op d@(pc,mem,inp,out) imm
  | op a b = memset d (pc+4) res 1
  | otherwise = memset d (pc+4) res 0
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = mem !! (pc + 3)

exec :: (Int,[Int],[Int],String) -> (Int,[Int],[Int],String)
exec d@(pc,mem,_,_)
  | (mem !! pc) == 99 = d
  | otherwise = exec $ step d

test :: String -> [Int] -> String
test bytes input = out
  where (_,_,_,out) = exec (0, parse bytes, input, [])

-- day 7 specific below ----

amp :: [Int] -> Int -> Int -> Int
amp mem index input = read out
  where (_,_,_,out) = exec (0, mem, [index, input], [])

amps :: [Int] -> Int -> [Int] -> Int
amps _ v [] = v
amps mem val (s:ss) = amps mem out ss
  where out = amp mem s val

permute :: [([Int],[Int])] -> [[Int]]
permute ps@((a,[]):as) = map fst ps
permute ps = permute $ concat $ map generate ps

generate :: ([Int],[Int]) -> [([Int],[Int])]
generate (a,b) = [ (a ++ [g], bs) | (g,bs) <- grab b]

grab :: [Int] -> [(Int,[Int])]
grab g = [ (a, filter (a/=) bs) | (a, bs) <- opts]
  where opts = zip g (take (length g) $ repeat g)

run bytes = maximum $ map (amps mem 0) $ permute [([],[0,1,2,3,4])]
  where mem = parse bytes

