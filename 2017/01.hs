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

run :: String -> [Int]
run str = [score 0 0 1 nums,
           score 0 0 (div (length nums) 2) nums]
    where nums = [read [n] | n <- str]
