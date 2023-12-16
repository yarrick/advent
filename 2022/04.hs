import Data.Char
import Data.List

part1 :: ([Int],[Int]) -> Bool
part1 ((a:b:[]),(c:d:[])) = c >= a && d <= b

part2 :: ([Int],[Int]) -> Bool
part2 ((a:b:[]),(c:d:[])) = c <= b

process :: [([Int],[Int])] -> [String]
process tasks = map (show.length.(\f -> filter f tasks)) [part1, part2]

-- sort pairs, earlier and bigger one first
parse :: String -> ([Int],[Int])
parse r
    | head a < head b = (a,b)
    | head a == head b && last a >= last b = (a,b)
    | otherwise = (b,a)
    where nums = map read $ filter (isDigit.head) $ groupBy (\x y -> isDigit x && isDigit y) r
          a = take 2 nums
          b = drop 2 nums

main :: IO ()
main = interact (unlines . process . map parse . lines)
