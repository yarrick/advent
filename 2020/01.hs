import Data.List
import Data.Maybe

part1 :: ([Int],[Int]) -> Int
part1 (lo,hi) = head $ summer 2020 lo hi

summer :: Int -> [Int] -> [Int] -> [Int]
summer _ [] _ = []
summer _ _ [] = []
summer goal (l:ls) (h:hs)
    | l + h == goal = [l * h]
    | l + h < goal = summer goal ls (h:hs)
    | l + h > goal = summer goal (l:ls) hs

part2 :: Int -> ([Int],[Int]) -> Int
part2 pos (lo,hi)
    | triplet == [] = part2 (pos+1) (lo,hi)
    | otherwise = base * (head triplet)
    where base = lo !! pos
          triplet = summer (2020 - base) (take pos lo ++ drop (pos+1) lo) hi

process :: [String] -> [String]
process rows = map show [part1 groups, part2 0 groups]
    where nums = map read rows
          groups = (sort nums, reverse $ sort nums)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

