module Device where

import Data.Bits

data Operation =
    AddReg | AddImm |
    MulReg | MulImm |
    AndReg | AndImm |
    OrReg  | OrImm  |
    SetReg | SetImm |
    GtIReg | GtRegI | GtRegReg |
    EqIReg | EqRegI | EqRegReg deriving (Eq,Show)

-- read from register (0-based)
gr :: [Int] -> Int -> Int
gr reg idx = reg !! idx

decode :: String -> Operation
decode "addr" = AddReg
decode "addi" = AddImm
decode "mulr" = MulReg
decode "muli" = MulImm
decode "banr" = AndReg
decode "bani" = AndImm
decode "borr" = OrReg
decode "bori" = OrImm
decode "setr" = SetReg
decode "seti" = SetImm
decode "gtir" = GtIReg
decode "gtri" = GtRegI
decode "gtrr" = GtRegReg
decode "eqir" = EqIReg
decode "eqri" = EqRegI
decode "eqrr" = EqRegReg

perform :: [Int] -> [Int] -> Operation -> [Int]
perform reg args op = (take pos reg) ++ [res] ++ (drop (pos+1) reg)
  where pos = last args
        res = compute reg (args!!0) (args!!1) op

compute :: [Int] -> Int -> Int -> Operation -> Int
compute regs a b AddReg = compute regs a (gr regs b) AddImm
compute regs a b AddImm = (gr regs a) + b
compute regs a b MulReg = compute regs a (gr regs b) MulImm
compute regs a b MulImm = (gr regs a) * b
compute regs a b AndReg = compute regs a (gr regs b) AndImm
compute regs a b AndImm = (.&.) (gr regs a) b
compute regs a b OrReg = compute regs a (gr regs b) OrImm
compute regs a b OrImm = (.|.) (gr regs a) b
compute regs a b SetReg = compute regs (gr regs a) b SetImm
compute regs a b SetImm = a
compute regs a b GtIReg
  | a > (gr regs b) = 1
  | otherwise = 0
compute regs a b GtRegI
  | (gr regs a) > b = 1
  | otherwise = 0
compute regs a b GtRegReg = compute regs (gr regs a) b GtIReg
compute regs a b EqIReg
  | a == (gr regs b) = 1
  | otherwise = 0
compute regs a b EqRegI
  | (gr regs a) == b = 1
  | otherwise = 0
compute regs a b EqRegReg = compute regs (gr regs a) b EqIReg

-- pcreg, code, regs
type State = (Int, [(Operation, [Int])], [Int])

exec :: State -> State
exec st = execmax (-1) st

-- exec max certain number of cycles
execmax :: Int -> State -> State
execmax _ (-1,_,_) = error "Need pcreg!"
execmax 0 st = st
execmax loops st@(pcreg,code,regs)
  | pc < 0 || pc >= length code = st
  | otherwise = execmax (loops-1) (pcreg, code, perform nextregs [pcreg,1,pcreg] AddImm)
  where pc = regs !! pcreg
        (op,args) = code !! pc
        nextregs = perform regs args op

parse :: [String] -> State
parse (s:ss)
  | take 3 s == "#ip" = (read $ drop 4 s, map readrow ss, startregs)
  | otherwise = (-1,map readrow (s:ss), startregs)
  where readrow row = (\(o:args) -> (decode o, map read args)) $ words row
        startregs = take 6 $ repeat 0
