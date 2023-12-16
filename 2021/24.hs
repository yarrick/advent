import Data.List
import qualified Data.Map as M

data Arg = Reg Char | Val Int deriving (Eq,Show)
data Op = Add Char Arg
        | Mul Char Arg
        | Div Char Arg
        | Mod Char Arg
        | Eql Char Arg
        | Inp Char deriving (Eq,Show)

-- ops, pc, regs, inputs
type Machine = ([Op], Int, [(Char, Int)], [Int])

parse :: [String] -> Op
parse (op:loc:more)
    | op == "add" = Add reg arg
    | op == "mul" = Mul reg arg
    | op == "div" = Div reg arg
    | op == "mod" = Mod reg arg
    | op == "eql" = Eql reg arg
    | op == "inp" = Inp reg
    where reg = head loc
          rreg = head $ head more
          arg
            | elem rreg "-0123456789" = Val (read $ head more)
            | otherwise = Reg rreg

readReg :: Machine -> Char -> Int
readReg (_,_,mem,_) reg
    | length regs == 0 = 0
    | otherwise = snd $ head regs
    where regs = filter (\(r,_) -> r == reg) mem

writeReg :: Machine  -> Char -> Int -> [(Char,Int)]
writeReg (_,_,mem,_) reg val = (reg,val) : filter (\(r,_) -> r /= reg) mem

getArg :: Machine -> Arg -> Int
getArg _ (Val v) = v
getArg m (Reg r) = readReg m r

store m@(ops,pc,mem,ins) reg val = (ops, pc+1, writeReg m reg val, ins)
storeOp m op reg arg = store m reg (op (readReg m reg) (getArg m arg))

execute :: Machine -> Op -> Machine
execute m (Add reg arg) = storeOp m (+) reg arg
execute m (Mul reg arg) = storeOp m (*) reg arg
execute m (Div reg arg) = storeOp m div reg arg
execute m (Mod reg arg)
    | readReg m reg < 0 = ([],999999,writeReg m 'z' 99,[])
    | otherwise = storeOp m rem reg arg
execute m (Eql reg arg) = store m reg res
    where res
            | readReg m reg == getArg m arg = 1
            | otherwise = 0
execute m@(ops,pc,_,ins) (Inp reg) = (ops, pc+1, writeReg m reg (head ins), tail ins)

run :: Machine -> Machine
run m@(ops,pc,_,_)
    | pc >= length ops = m
    | otherwise = run $ execute m (ops !! pc)

matches :: [((Int,Int),Int)] -> ([Op],Int) -> [((Int,Int),Int)]
matches prev (ops,attempts) = filter (\(_,g) -> M.member g goal) $ map zreg $ filter valid tries
    where tries = [ ((inp,z),run (ops,0,[('z',z)],[inp])) |
                    z <- [-999..attempts], inp <- [1..9] ]
          valid ((inp,z),(ps,pc,_,_)) = pc < 1000
          zreg (a,m) = (a,readReg m 'z')
          goal = M.fromList $ zip (nub $ map (snd.fst) prev) (repeat 1)

inputs :: [Op] -> [[Op]]
inputs [] = []
inputs (o:os) = (o:og) : inputs next
    where (og,next) = break isInp os
          isInp (Inp _) = True
          isInp _ = False

result d _ _ [] = [d]
result d z fn (f:fs) = concatMap (\((dig,_),nz) -> result (d++[dig]) nz fn fs) keys
    where keys = fn $ sort $ filter (\((_,mz),_) -> mz == z) f

process :: [Op] -> [String]
process ops = [solve reverse, solve id]
    where parts = reverse $ inputs ops
          -- hacky per-step iteration counts to speed things up. Up to 5M needed to find a solution.
          fits = tail $ scanl matches [((0,0),0)] $ zip parts [100,1000,10000,250000,10000,250000,5000000,
                                                               250000,10000,500,10000,500,500,500]
          solve fn = concatMap show $ head $ result [] 0 fn $ reverse fits

main :: IO ()
main = interact (unlines . process . map (parse.words) . lines)
