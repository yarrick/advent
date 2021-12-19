import Data.List
import Intcode

test :: String -> [Integer] -> ([Integer],[Integer])
test bytes input = (take 100 (memory st), (outdata st))
  where st = exec $ newstate (parse bytes) input

run :: String -> [Integer]
run bytes = concatMap (\i -> outdata $ exec $ newstate (parse bytes) i) [[1],[2]]
