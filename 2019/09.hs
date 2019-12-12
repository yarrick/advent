import Data.List
import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

test :: String -> [Integer] -> ([Integer],[Integer])
test bytes input = (take 100 mem, out)
  where (_,mem,_,out,_,_) = exec $ newstate (parse bytes) input

run :: String -> [Integer] -> [Integer]
run bytes input = out
  where (_,_,_,out,_,_) = exec $ newstate (parse bytes) input
