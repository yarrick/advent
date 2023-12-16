import Data.List

pickPos :: [Int] -> Int
pickPos state = snd $ head $ sortBy rank placed
    where placed = zip state [0..]
          rank (sa,pa) (sb,pb)
            | sa == sb = compare pa pb
            | otherwise = compare sb sa -- descending sort

add :: [Int] -> [Int] -> [Int]
add st ad
    | len < length ad = add (inc st (take len ad)) (drop len ad)
    | otherwise = inc st ad
    where len = length st
          inc [] [] = []
          inc a [] = a
          inc (a:as) (b:bs) = (a+b) : add as bs

run :: [Int] -> [[Int]] -> ([Int],Int)
run state hist
    | elem state hist = (state, length hist)
    | otherwise = run (add base adds) (state:hist)
    where pos = pickPos state
          val = state !! pos
          adds = (replicate (pos+1) 0) ++ (replicate val 1)
          base = take pos state ++ [0] ++ drop (pos+1) state

process :: [String] -> [String]
process rows = map show [cycles, cycles2]
    where nums = map read $ concatMap words rows
          (repeated, cycles) = run nums []
          (repeated2, cycles2) = run repeated []

main :: IO ()
main = interact (unlines . process . lines)
