
part1 :: Int -> [Int] -> Int
part1 num (a:[]) = num
part1 deeper (a:b:cc)
    | b > a = part1 (succ deeper) (b:cc)
    | otherwise = part1 deeper (b:cc)

part2 :: Int -> [Int] -> Int
part2 deeper nums
    | length nums < 4 = deeper
    | b > a = part2 (succ deeper) (tail nums)
    | otherwise = part2 deeper (tail nums)
    where a = sum $ take 3 nums
          b = sum $ take 3 $ tail nums

process :: [String] -> [String]
process rows = map show [part1 0 nums, part2 0 nums]
    where nums = map read rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

