import Data.List
import Data.Maybe

part1 :: ([Int],[Int]) -> Int
part1 (lo,hi) = fromJust $ summer 2020 lo hi

summer :: Int -> [Int] -> [Int] -> Maybe Int
summer _ [] _ = Nothing
summer _ _ [] = Nothing
summer goal (l:ls) (h:hs)
    | l + h == goal = Just (l * h)
    | l + h < goal = summer goal ls (h:hs)
    | l + h > goal = summer goal (l:ls) hs

part2 :: Int -> ([Int],[Int]) -> Int
part2 pos (lo,hi)
    | isJust triplet = base * fromJust triplet
    | otherwise = part2 (pos+1) (lo,hi)
    where base = lo !! pos
          triplet = summer (2020 - base) lo hi

process :: [String] -> [String]
process rows = map show [part1 groups, part2 0 groups]
    where nums = sort $ map read rows
          groups = (nums, reverse nums)

main :: IO ()
main = interact (unlines . process . lines)
