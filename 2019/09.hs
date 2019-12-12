import Data.List
import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

test :: String -> [Integer] -> ([Integer],[Integer])
test bytes input = (take 100 (memory st), (outdata st))
  where st = exec $ newstate (parse bytes) input

run :: String -> [Integer] -> [Integer]
run bytes input = outdata $ exec $ newstate (parse bytes) input
