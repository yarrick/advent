import Data.List

data Op = Inc | Dec deriving (Show, Eq)

data Cmp = Less | Greater | GreaterEqual | LessEqual | Equal | NotEqual deriving (Show, Eq)

type Instruction = ((String,Op,Int),(String,Cmp,Int))

parse :: [String] -> Instruction
parse (wr:operator:wn:"if":rr:cmp:rn:xs) = ((wr,op,read wn), (rr,cmper,read rn))
    where op
            | operator == "inc" = Inc
            | otherwise = Dec
          cmper
            | cmp == ">" = Greater
            | cmp == ">=" = GreaterEqual
            | cmp == "<" = Less
            | cmp == "<=" = LessEqual
            | cmp == "==" = Equal
            | cmp == "!=" = NotEqual

type Register = (String, Int)
-- keep highest ever value
type Memory = ([Register], Int)

newMem = ([], -999999)
regNamed name (x,_) = x == name

readReg :: Memory -> String -> Int
readReg (regs,_) name
    | length match == 0 = 0
    | otherwise = snd $ head match
    where match = filter (regNamed name) regs

writeReg :: Memory -> Register -> Memory
writeReg (regs,max) upd@(name,val) = (upd : other, newmax)
    where (old, other) = partition (regNamed name) regs
          newmax = maximum [max, val]

predicate :: Int -> Cmp -> Int -> Bool
predicate a Less b = a < b
predicate a LessEqual b = a <= b
predicate a Greater b = a > b
predicate a GreaterEqual b = a >= b
predicate a Equal b = a == b
predicate a NotEqual b = a /= b

eval :: Memory -> Instruction -> Memory
eval regs ((wr,op,wn),(rr,cmp,rn))
    | pred = writeReg regs (wr, wval)
    | otherwise = regs
    where pval = readReg regs rr
          pred = predicate pval cmp rn
          val = readReg regs wr
          wval
            | op == Inc = val + wn
            | op == Dec = val - wn

run :: Memory -> [Instruction] -> Memory
run regs ops = foldl eval regs ops

process :: [String] -> [String]
process rows = map show [maximum $ map snd endreg, max]
    where ops = map (parse . words) rows
          (endreg,max) = run newMem ops

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

