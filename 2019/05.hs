import Intcode

run :: String -> [Integer]
run bytes = outdata $ exec $ newstate (parse bytes) $ repeat 1

-- part 2

run2 :: String -> [Integer]
run2 bytes = outdata $ exec $ newstate (parse bytes) $ repeat 5

