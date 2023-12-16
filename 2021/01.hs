solve :: Int -> Int -> [Int] -> Int
solve glen deeper nums
    | length nums < succ glen = deeper
    | b > a = solve glen (succ deeper) (tail nums)
    | otherwise = solve glen deeper (tail nums)
    where a = sum $ take glen nums
          b = sum $ take glen $ tail nums

process :: [String] -> [String]
process rows = map show [solve 1 0 nums, solve 3 0 nums]
    where nums = map read rows

main :: IO ()
main = interact (unlines . process . lines)

