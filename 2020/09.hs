misses :: [Int] -> [Int] -> Int
misses pre (x:xs)
    | elem x sums = misses newpre xs
    | otherwise = x
    where prerange = pred $ length pre
          sums = [(pre !! a) + (pre !! b) | a <- [0..prerange], b <- [0..prerange], a /= b ]
          newpre = x : (take prerange pre)

sumrange :: Int -> Int -> [Int] -> [Int] -> [Int]
sumrange goal score guess nums
    | score == goal = guess
    | score > goal = sumrange goal (score - head guess) (tail guess) nums
    | score < goal = sumrange goal (score + cand) (guess ++ [cand]) (tail nums)
    where cand = head nums

process :: [String] -> [String]
process rows = map show $ [firstmiss, minimum rangescore + maximum rangescore]
    where nums = map read rows
          getmiss n prelen = misses (reverse $ take prelen n) (drop prelen n)
          firstmiss = getmiss nums preamble
          rangescore = sumrange firstmiss 0 [] nums
          preamble
            | length nums < 25 = 5 -- for example data
            | otherwise = 25

main :: IO ()
main = interact (unlines . process . lines)
