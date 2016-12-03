import Data.List

check :: [Int] -> Bool
check nums = (last all) < (sum $ take 2 all)
  where all = sort nums

parse :: String -> [Int]
parse str = map read $ words str

part1 :: [String] -> String
part1 rows = show $ length $ filter (==True) $ map check $ map parse rows

-- part 2
check3 :: [String] -> Int
check3 [] = 0
check3 (a:b:c:dd) = (length $ filter (==True) [left,mid,right]) + check3 dd
  where pa = parse a
        pb = parse b
        pc = parse c
        left  = check [pa!!0, pb!!0, pc!!0]
        mid   = check [pa!!1, pb!!1, pc!!1]
        right = check [pa!!2, pb!!2, pc!!2]

part2 :: [String] -> String
part2 rows = show $ check3 rows

process :: [String] -> [String]
process rows = [part1 rows, part2 rows]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
