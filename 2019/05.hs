import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

run :: String -> [Integer]
run bytes = out
  where (pc,mem,inp,out,_,_) = exec $ newstate (parse bytes) $ repeat 1

-- part 2

run2 :: String -> [Integer]
run2 bytes = out
  where (pc,mem,inp,out,_,_) = exec $ newstate (parse bytes) $ repeat 5

