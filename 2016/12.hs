import Asmbunny

run str start = reg
  where (_,_,reg,_) = runVM $ newVM (readInstr $ words str) start

process :: String -> [String]
process rows = map (show.head) [run rows [0,0,0,0], run rows [0,0,1,0]]

main :: IO ()
main = interact (unlines . process)
