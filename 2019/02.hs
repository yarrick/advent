import Intcode

program :: Int -> [Integer] -> [Integer]
program num mem = head mem : toInteger noun : toInteger verb : (drop 3 mem)
    where (noun, verb) = quotRem num 100

memzero :: State -> Integer
memzero st = (memory st) !! 0

run :: String -> Integer
run bytes = memzero $ exec $ newstate (program 1202 $ parse bytes) []

-- part 2

run2 bytes
    | matches == [] = 0
    | otherwise = head matches
    where matches = map snd $ take 1 $ filter (\a -> snd a == 19690720) $
                    map (\prog -> (prog, memzero (exec $ newstate (program prog $ parse bytes) []))) [0..9999]

process :: String -> [String]
process rows = [show $ run rows, show $ run2 rows]

main :: IO ()
main = interact (unlines . process)
