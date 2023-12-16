import Data.List

parse :: String -> [Int]
parse str = map read $ words str

run :: [[Int]] -> Int
run [] = 0
run (r:rs) = (high-low) + run rs
    where nums = sort r
          low = head nums
          high = head $ reverse nums

divisors :: [Int] -> Int
divisors (r:rs)
  | length divs > 0 = div (fst $ head divs) r
  | otherwise = divisors rs
  where divs = filter (\(a,b) -> b == 0) [(x, rem x r) | x <- rs ]

run2 :: [[Int]] -> Int
run2 [] = 0
run2 (r:rs) = divisors (sort r) + run2 rs

process :: [String] -> [String]
process rows = [show $ run nums, show $ run2 nums]
    where nums = map parse rows

main :: IO ()
main = interact (unlines . process . lines)
