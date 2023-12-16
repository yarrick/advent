import Data.List

steps :: [Int] -> Int -> Int -> (Int -> Int) -> Int
steps jumps count pos fn
    | pos >= length jumps = count
    | otherwise = steps nextjumps (count+1) nextpos fn
    where val = jumps !! pos
          nextpos = pos + val
          nextjumps = take pos jumps ++ [fn val] ++ drop (pos+1) jumps

process :: [String] -> [String]
process rows = map show [steps nums 0 0 succ, steps nums 0 0 nextjump]
    where nums = map read rows
          nextjump x
            | x >= 3 = x-1
            | otherwise = x+1

main :: IO ()
main = interact (unlines . process . lines)
