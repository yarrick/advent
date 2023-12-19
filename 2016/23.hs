import Asmbunny

regA :: VM -> Int
regA (_,_,regs,_) = head regs

run str = regA $ runVM $ newVM (readInstr $ words str) [7,0,0,0]

run2 str = regA $ runVMopt $ newVM (readInstr $ words str) [12,0,0,0]

process :: String -> [String]
process rows = map (show) [run rows, run2 rows]

main :: IO ()
main = interact (unlines . process)
