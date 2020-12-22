import Control.DeepSeq

data Op = Set Char Int
        | SetReg Char Char
        | Add Char Int
        | AddReg Char Char
        | SubReg Char Char
        | Mul Char Int
        | MulReg Char Char
        | JumpNZ Int Int
        | JumpNZReg Char Int deriving (Eq,Show)

-- ops, pc, regs, muls
type Machine = ([Op], Int, [(Char, Int)], Int)

parse :: [String] -> Op
parse (op:loc:more)
    | op == "set" && immediate = Set reg num
    | op == "set" = SetReg reg rreg
    | op == "add" && immediate = Add reg num
    | op == "add" = AddReg reg rreg
    | op == "sub" && immediate = Add reg (negate num)
    | op == "sub" = SubReg reg rreg
    | op == "mul" && immediate = Mul reg num
    | op == "mul" = MulReg reg rreg
    | op == "jnz" && elem (head loc) digits = JumpNZ (read loc) num
    | op == "jnz" = JumpNZReg reg num
    where reg = head loc
          rreg = head $ head more
          digits = "-0123456789"
          immediate = elem rreg digits
          num = read $ head more

readReg :: [(Char,Int)] -> Char -> Int
readReg mem reg
    | length regs == 0 = 0
    | otherwise = snd $ head regs
    where regs = filter (\(r,_) -> r == reg) mem

writeReg :: [(Char,Int)] -> Char -> Int -> [(Char,Int)]
writeReg mem reg val = deepseq newmem newmem
    where newmem = (reg,val) : filter (\(r,_) -> r /= reg) mem

execute :: Machine -> Op -> Machine
execute (ops,pc,mem,muls) (Set reg val) = (ops, pc+1, writeReg mem reg val, muls)
execute m@(_,_,mem,_) (SetReg reg vreg) = execute m (Set reg (readReg mem vreg))
execute (ops,pc,mem,muls) (Add reg val) = (ops, pc+1, writeReg mem reg (val + readReg mem reg), muls)
execute m@(_,_,mem,_) (AddReg reg vreg) = execute m (Add reg (readReg mem vreg))
execute m@(_,_,mem,_) (SubReg reg vreg) = execute m (Add reg (negate $ readReg mem vreg))
execute (ops,pc,mem,muls) (Mul reg val) = (ops, pc+1, writeReg mem reg (val * readReg mem reg), succ muls)
execute m@(_,_,mem,_) (MulReg reg vreg) = execute m (Mul reg (readReg mem vreg))
execute (ops,pc,mem,muls) (JumpNZ cond val)
    | cond /= 0 = (ops, pc+val, mem, muls)
    | otherwise = (ops, pc+1, mem, muls)
execute m@(_,_,mem,_) (JumpNZReg reg num) = execute m (JumpNZ (readReg mem reg) num)


run :: Machine -> Machine
run m@(ops,pc,_,_)
    | pc >= length ops = m
    | otherwise = run $ execute m (ops !! pc)

process :: [Op] -> [String]
process ops = [show muls]
    where (_,_,_,muls) = run (ops, 0, [], 0)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map (parse.words) . lines)

