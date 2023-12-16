import Data.List
import Data.Char

parse :: [String] -> [Int]
parse (s:arrow:nums) = nub $ (read s) : map get nums
    where get t = read $ takeWhile isDigit t

connected :: [Int] -> [Int] -> Bool
connected g h = any (\x -> elem x g) h

build :: [[Int]] -> [Int] -> [[Int]]
build groups g
    | length matches > 0 = (nub $ concat (g:matches)) : other
    | otherwise = g : groups
    where (matches,other) = partition (connected g) groups

process :: [[Int]]  -> [String]
process groups = map show [length $ head $ filter (elem 0) nets, length $ nets]
    where nets = foldl build [] groups

main :: IO ()
main = interact (unlines . process . (map (parse . words)) . lines)
