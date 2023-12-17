import Data.List
import Intcode

test :: String -> [Integer] -> ([Integer],[Integer])
test bytes input = (take 100 (memory st), (outdata st))
  where st = exec $ newstate (parse bytes) input

run :: String -> [Integer]
run bytes = concatMap (\i -> outdata $ exec $ newstate (parse bytes) i) [[1],[2]]

process :: String -> [String]
process rows = map show (run rows)

main :: IO ()
main = interact (unlines . process)
