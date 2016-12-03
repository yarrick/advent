import Data.List

check :: [Int] -> Bool
check nums = (last all) < (sum $ take 2 all)
  where all = sort nums

parse :: String -> [Int]
parse str = map read $ words str

check3 :: [String] -> Int
check3 [] = 0
check3 (a:b:c:dd) = (length $ filter (==True) [left,mid,right]) + check3 dd
  where pa = parse a
        pb = parse b
        pc = parse c
        left  = check [pa!!0, pb!!0, pc!!0]
        mid   = check [pa!!1, pb!!1, pc!!1]
        right = check [pa!!2, pb!!2, pc!!2]

process :: [String] -> [String]
process rows = [show $ check3 rows]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
