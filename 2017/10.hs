import Knot

part1 :: [Int] -> Int
part1 twists = product $ take 2 $ run twists

part2 :: String -> String
part2 = knot

process :: [String] -> [String]
process (row:_) = [show $ part1 nums, part2 row]
    where nums = read $ "[" ++ row ++ "]"

main :: IO ()
main = interact (unlines . process . lines)
