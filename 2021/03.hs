import Data.Char
import Data.List

closest :: Int -> (Int -> Int -> Bool) -> [[Int]] -> [Int]
closest _ _ [a] = a
closest n op cands = closest (succ n) op $ filter good cands
    where goal = part1 cands op
          good nums = goal !! n == nums !! n

part2 :: [[Int]] -> (Int -> Int -> Bool) -> [Int]
part2 bits op = closest 0 op bits

part1 :: [[Int]] -> (Int -> Int -> Bool) -> [Int]
part1 bits op = map (picker op) $ map (\r -> (blen r 0, blen r 1)) $ transpose bits
    where blen l val = length $ fst $ partition (==val) l

picker :: (Int -> Int -> Bool) -> (Int, Int) -> Int
picker op (a,b)
    | op a b = 0
    | otherwise = 1

process :: [String] -> [String]
process rows = map (show.solve) [part1, part2]
    where bits = map (map digitToInt) rows
          deci n = foldl (\a b -> 2*a+b) 0 n
          solve p = product $ map (deci.p bits) [(>), (<=)]

main :: IO ()
main = interact (unlines . process . lines)

