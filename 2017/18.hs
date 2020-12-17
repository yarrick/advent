import Control.DeepSeq

data Op = Sound Char
        | Set Char Int
        | SetReg Char Char
        | Add Char Int
        | AddReg Char Char
        | Mul Char Int
        | MulReg Char Char
        | Mod Char Int
        | ModReg Char Char
        | Recover Char
        | JumpGZ Char Int
        | JumpGZReg Char Char deriving (Eq,Show)

-- ops, pc, regs, freq
type Machine = ([Op], Int, [(Char, Int)], Int)

parse :: [String] -> Op
parse (op:loc:more)
    | op == "snd" = Sound reg
    | op == "set" && immediate = Set reg num
    | op == "set" = SetReg reg rreg
    | op == "add" && immediate = Add reg num
    | op == "add" = AddReg reg rreg
    | op == "mul" && immediate = Mul reg num
    | op == "mul" = MulReg reg rreg
    | op == "mod" && immediate = Mod reg num
    | op == "mod" = ModReg reg rreg
    | op == "rcv" = Recover reg
    | op == "set" = Set reg num
    | op == "jgz" && immediate = JumpGZ reg num
    | op == "jgz" = JumpGZReg reg rreg
    where reg = head loc
          rreg = head $ head more
          immediate = elem rreg "-0123456789"
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
execute (ops,pc,mem,_) (Sound reg) = (ops, pc+1, mem, readReg mem reg)
execute (ops,pc,mem,freq) (Set reg val) = (ops, pc+1, writeReg mem reg val, freq)
execute m@(_,_,mem,_) (SetReg reg vreg) = execute m (Set reg (readReg mem vreg))
execute (ops,pc,mem,freq) (Add reg val) = (ops, pc+1, writeReg mem reg (val + readReg mem reg), freq)
execute m@(_,_,mem,_) (AddReg reg vreg) = execute m (Add reg (readReg mem vreg))
execute (ops,pc,mem,freq) (Mul reg val) = (ops, pc+1, writeReg mem reg (val * readReg mem reg), freq)
execute m@(_,_,mem,_) (MulReg reg vreg) = execute m (Mul reg (readReg mem vreg))
execute (ops,pc,mem,freq) (Mod reg val) = (ops, pc+1, writeReg mem reg (readReg mem reg `mod` val), freq)
execute m@(_,_,mem,_) (ModReg reg vreg) = execute m (Mod reg (readReg mem vreg))
execute (ops,pc,mem,freq) (Recover reg)
    | readReg mem reg > 0 = (ops, 10000, mem, freq)
    | otherwise = (ops, pc+1, mem, freq)
execute (ops,pc,mem,freq) (JumpGZ reg val)
    | (readReg mem reg) > 0 = (ops, pc+val, mem, freq)
    | otherwise = (ops, pc+1, mem, freq)
execute m@(_,_,mem,_) (JumpGZReg reg vreg) = execute m (JumpGZ reg (readReg mem vreg))

run :: Machine -> Int
run m@(ops,pc,mem,freq)
    | pc == 10000 = freq
    | otherwise = run $ execute m (ops !! pc)

process :: [Op] -> [String]
process ops = [show $ run machine]
    where machine = (ops, 0, [], 0)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map (parse.words) . lines)

