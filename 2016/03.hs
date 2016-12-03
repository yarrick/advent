import Data.List

check :: [Int] -> Bool
check nums = (last all) < (sum $ take 2 all)
  where all = sort nums

parse :: String -> [Int]
parse str = map read $ words str

part1 :: [String] -> Int
part1 rows = length $ filter (==True) $ map (check . parse) rows

part2 :: [String] -> Int
part2 [] = 0
part2 (a:b:c:dd) = (length $ filter (==True) [ccheck 0,ccheck 1,ccheck 2]) + part2 dd
  where ccheck x = check [(parse a)!!x, (parse b)!!x, (parse c)!!x]

process :: [String] -> [String]
process rows = map show [part1 rows, part2 rows]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
