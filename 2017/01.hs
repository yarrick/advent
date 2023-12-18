import Data.List

getpos :: [Int] -> Int -> Int
getpos nums pos = nums !! (mod pos (length nums))

score :: Int -> Int -> Int -> [Int] -> Int
score pos s offs nums
    | pos >= length nums = s
    | hit = score (pos+1) (s+curr) offs nums
    | otherwise = score (pos+1) s offs nums
    where curr = getpos nums pos
          hit = curr == getpos nums (pos+offs)

process :: [String] -> [String]
process (row:_) = map show [run 1, run (div (length nums) 2)]
    where nums = [read [n] | n <- row]
          run n = score 0 0 n nums

main :: IO ()
main = interact (unlines . process . lines)
