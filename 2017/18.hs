import Control.DeepSeq
import Data.Char

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
        | Send Int
        | SendReg Char
        | Receive Char
        | JumpGZ Char Int
        | JumpGZReg Char Char deriving (Eq,Show)

-- ops, pc, regs, freq/sendcount, recvq, sendq
type Machine = ([Op], Int, [(Char, Int)], Int, [Int], [Int])

machine :: [Op] -> Int -> Machine
machine ops pid = (ops, 0, [('p',pid)], 0, [], [])

parse :: Int -> [String] -> Op
parse part (op:loc:more)
    | part == 1 && op == "snd" = Sound reg
    | op == "snd" && elem reg "-0123456789" = Send (read loc)
    | op == "snd" = SendReg reg
    | op == "set" && immediate = Set reg num
    | op == "set" = SetReg reg rreg
    | op == "add" && immediate = Add reg num
    | op == "add" = AddReg reg rreg
    | op == "mul" && immediate = Mul reg num
    | op == "mul" = MulReg reg rreg
    | op == "mod" && immediate = Mod reg num
    | op == "mod" = ModReg reg rreg
    | part == 1 && op == "rcv" = Recover reg
    | op == "rcv" = Receive reg
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
execute (ops,pc,mem,_,rq,sq) (Sound reg) = (ops, pc+1, mem, readReg mem reg, rq, sq)
execute (ops,pc,mem,freq,rq,sq) (Set reg val) = (ops, pc+1, writeReg mem reg val, freq, rq, sq)
execute m@(_,_,mem,_,_,_) (SetReg reg vreg) = execute m (Set reg (readReg mem vreg))
execute (ops,pc,mem,freq,rq,sq) (Add reg val) = (ops, pc+1, writeReg mem reg (val + readReg mem reg), freq, rq, sq)
execute m@(_,_,mem,_,_,_) (AddReg reg vreg) = execute m (Add reg (readReg mem vreg))
execute (ops,pc,mem,freq,rq,sq) (Mul reg val) = (ops, pc+1, writeReg mem reg (val * readReg mem reg), freq, rq, sq)
execute m@(_,_,mem,_,_,_) (MulReg reg vreg) = execute m (Mul reg (readReg mem vreg))
execute (ops,pc,mem,freq,rq,sq) (Mod reg val) = (ops, pc+1, writeReg mem reg (readReg mem reg `mod` val), freq, rq, sq)
execute m@(_,_,mem,_,_,_) (ModReg reg vreg) = execute m (Mod reg (readReg mem vreg))
execute (ops,pc,mem,freq,rq,sq) (Recover reg)
    | readReg mem reg > 0 = (ops, 10000, mem, freq, rq, sq)
    | otherwise = (ops, pc+1, mem, freq, rq, sq)
execute (ops,pc,mem,freq,rq,sq) (JumpGZ reg val)
    | isDigit reg && digitToInt reg > 0 =  (ops, pc+val, mem, freq, rq, sq)
    | (readReg mem reg) > 0 = (ops, pc+val, mem, freq, rq, sq)
    | otherwise = (ops, pc+1, mem, freq, rq, sq)
execute m@(_,_,mem,_,_,_) (JumpGZReg reg vreg) = execute m (JumpGZ reg (readReg mem vreg))
execute (ops,pc,mem,sends,rq,sq) (Send val) = (ops, pc+1, mem, succ sends, rq, sq ++ [val])
execute m@(_,_,mem,_,_,_) (SendReg reg) = execute m (Send (readReg mem reg))
execute (ops,pc,mem,sends,rq,sq) (Receive reg)
    | rq == [] = error "Tried receive with empty queue"
    | otherwise = (ops, pc+1, writeReg mem reg (head rq), sends, tail rq, sq)

run1 :: Machine -> Int
run1 m@(ops,pc,mem,freq,_,_)
    | pc >= length ops = freq
    | otherwise = run1 $ execute m (ops !! pc)

tryRun :: Machine -> (Bool, Machine)
tryRun m@(ops,pc,_,_,rq,_)
        | pc >= length ops = (False, m)
        | isReceive (ops !! pc) && rq == [] = (False, m)
        | otherwise = (True, execute m (ops !! pc))
        where isReceive (Receive _) = True
              isReceive _ = False

moveq :: Machine -> Machine -> (Machine, Machine)
moveq (oa,pa,ma,fa,rqa,sqa) (ob,pb,mb,fb,rqb,sqb) = ((oa,pa,ma,fa,rqa++sqb,[]), (ob,pb,mb,fb,rqb++sqa,[]))

run2 :: (Machine, Machine) -> Int
run2 (a,b)
    | arun || brun = run2 (newa, newb)
    | otherwise = sends b
    where (aq,bq) = moveq a b
          ((arun,newa):(brun,newb):_) = map tryRun [aq,bq]
          sends (_,_,_,sc,_,_) = sc

process :: [String] -> [String]
process rows = map show [run1 $ machine p1ops 0, run2 (machine p2ops 0, machine p2ops 1)]
    where p1ops = map (parse 1.words) rows
          p2ops = map (parse 2.words) rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

