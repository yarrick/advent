
parse :: String -> [Int]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

step :: Int -> [Int] -> [Int]
step pc mem
  | op == 1 = compute (+) pc mem
  | op == 2 = compute (*) pc mem
  | otherwise = []
    where op = mem !! pc

compute :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
compute op pc mem = take res mem ++ [op a b] ++ drop (res + 1) mem
    where
      a = mem !! (mem !! (pc + 1))
      b = mem !! (mem !! (pc + 2))
      res = mem !! (pc + 3)

run :: String -> Int
run bytes = head $ exec 0 $ parse bytes

exec :: Int -> [Int] -> [Int]
exec pc mem
  | (mem !! pc) == 99 = mem
  | otherwise = exec (pc+4) $ step pc mem

-- part 2

program :: Int -> [Int] -> [Int]
program num mem = head mem : noun : verb : drop 3 mem
    where (noun, verb) = quotRem num 100

run2 bytes = fst $ head $ filter (\a -> snd a == 19690720) $
  map (\prog -> (prog, head (exec 0 $ program prog $ parse bytes))) [0..9999]
