import Data.Maybe (fromJust)

data MathOp = Half Char | Triple Char | Incr Char deriving Show
data JumpOp = Jump Int | JumpEven Char Int | JumpOdd Char Int deriving Show
data Instr = Math MathOp | Jumper JumpOp deriving Show

getint :: String -> Int
getint ('+':ss) = read ss
getint s = read s

parse:: [String] -> [Instr]
parse [] = []
parse ("hlf":reg:ss) = Math (Half (head reg)) : parse ss
parse ("tpl":reg:ss) = Math (Triple (head reg)) : parse ss
parse ("inc":reg:ss) = Math (Incr (head reg)) : parse ss
parse ("jmp":off:ss) = Jumper (Jump (getint off)) : parse ss
parse ("jie":reg:off:ss) = Jumper (JumpEven (head reg) (getint off)) : parse ss
parse ("jio":reg:off:ss) = Jumper (JumpOdd (head reg) (getint off)) : parse ss

domath :: (Int -> Int) -> Char -> [(Char,Int)] -> [(Char,Int)]
domath fun c ((r,v):rr)
	| r == c = (r, (fun v)) : rr
	| otherwise = (r,v) : domath fun c rr

apply :: MathOp -> [(Char,Int)] -> [(Char,Int)]
apply (Half c) regs = domath (\x -> x `div` 2) c regs
apply (Triple c) regs = domath (*3) c regs
apply (Incr c) regs = domath (+1) c regs

condjump :: Int -> [(Char,Int)] -> (Int -> Bool) -> Char -> Int -> Int
condjump pc regs fun reg val
	| fun (fromJust $ lookup reg regs) = pc + val
	| otherwise = pc + 1

jump :: JumpOp -> Int -> [(Char,Int)] -> Int
jump (Jump val) pc _ = pc + val
jump (JumpEven reg val) pc regs = condjump pc regs even reg val
jump (JumpOdd reg val) pc regs = condjump pc regs (==1) reg val

doInstr :: Instr -> Int -> [(Char,Int)] -> (Int, [(Char,Int)])
doInstr (Jumper j) pc regs = (jump j pc regs, regs)
doInstr (Math m) pc regs = (pc + 1, apply m regs)

step :: [Instr] -> Int -> [(Char,Int)] -> [(Char,Int)]
step instr pc regs
	| pc < 0 || pc >= (length instr) = regs
	| otherwise = step instr newpc newregs
	where
		(newpc,newregs) = doInstr i pc regs
		i = instr !! pc

run :: String -> [(Char,Int)]
run str = step instr 0 [('a',0),('b',0)]
	where instr = parse $ words str

--part 2
run2 :: String -> [(Char,Int)]
run2 str = step instr 0 [('a',1),('b',0)]
	where instr = parse $ words str
