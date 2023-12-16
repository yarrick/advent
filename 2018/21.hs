import Device
import qualified Data.Map as M
import qualified Data.Vector as V

process :: State -> [String]
process st@(_,code,_) = map show [head r0s, looped M.empty 0 r0s]
    where instr = head $ filter (\(op,args) -> op == EqRegReg && elem 0 (take 2 args)) $ V.toList code
          r0s = zerocmp instr st

looped :: M.Map Int Int -> Int -> [Int] -> Int
looped mm prev (c:cs)
    | M.member c mm = prev
    | otherwise = looped (M.insert c 0 mm) c cs

zerocmp :: (Operation,[Int]) -> State -> [Int]
zerocmp instr st = (regs !! 3) : zerocmp instr (pcreg,code,regs)
    where (pcreg,code,regs) = execuntil instr st

main :: IO ()
main = interact (unlines . process . parse . lines)
