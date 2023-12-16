data Instr =
    Acc Int |
    Nop Int |
    Jump Int deriving (Eq, Show)

-- instructions, position, accumulator, past positions
type VM = ([Instr], Int, Int, [Int])

readArg :: String -> Int
readArg ('+':num) = read num
readArg ('-':num) = negate $ read num

readInstr :: [String] -> [Instr]
readInstr [] = []
readInstr ("acc":a:xs) = Acc (readArg a) : readInstr xs
readInstr ("jmp":a:xs) = Jump (readArg a) : readInstr xs
readInstr ("nop":a:xs) = Nop (readArg a) : readInstr xs

newVM :: [Instr] -> VM
newVM instr = (instr,0,0,[])

stepVM :: VM -> Instr -> VM
stepVM (code,pc,acc,pp) (Acc a) = (code,pc+1,acc+a,pc:pp)
stepVM (code,pc,acc,pp) (Jump a) = (code,pc+a,acc,pc:pp)
stepVM (code,pc,acc,pp) (Nop a) = (code,pc+1,acc,pc:pp)

toggle :: VM -> Int -> VM
toggle vm@(code,pc,acc,pp) pos = (take pos code ++ [changeOp op] ++ drop (pos+1) code,pc,acc,pp)
    where op = code !! pos
          changeOp (Acc x) = Acc x
          changeOp (Nop x) = Jump x
          changeOp (Jump x) = Nop x

runVM :: VM -> VM
runVM vm@(instr,pc,_,pos)
  | elem pc pos = vm
  | pc >= length instr = vm
  | otherwise = runVM $ stepVM vm (instr !! pc)

tryVMs :: VM -> Int -> VM
tryVMs vm pos
    | pc >= length code = result
    | otherwise = tryVMs vm (pos + 1)
    where modded = toggle vm pos
          result@(code,pc,acc,_) = runVM modded

process :: [String] -> [String]
process rows = map (show . acc) [runVM vm, tryVMs vm 0]
    where ops = readInstr $ concatMap words rows
          vm = newVM ops
          acc (_,_,a,_) = a

main :: IO ()
main = interact (unlines . process . lines)
