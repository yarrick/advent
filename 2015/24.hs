import Data.List

addone :: Int -> Int -> [[Int]] -> Int -> [[[Int]]]
addone limit val groups pos
    | pos == length groups = []
    | length (groups !! pos) == 0 = [grown]
    | fits = grown : addone limit val groups (pos+1)
    | otherwise = addone limit val groups (pos+1)
    where fits = sum (groups !! pos) + val <= limit
          grown = (take pos groups) ++ [(groups !! pos) ++ [val]] ++ (drop (pos+1) groups)

combos :: Int -> [[Int]] -> [Int] -> [[[Int]]]
combos _ a [] = [a]
combos limit prev (i:is)
    | length added > 0 = concat $ map (\a -> combos limit a is) added
    | otherwise = []
    where added = addone limit i prev 0

score :: [[Int]] -> (Int,Int)
score b = (length b1, product b1)
    where b1 = head $ sortBy (\x y -> compare (length x) (length y)) b

better :: (Int,Int,Int) -> [(Int,Int)] -> [(Int,Int)]
better _ [] = []
better (bestsize,bestent,skips) (x@(s,e):xs)
    | s < bestsize = x : better (s,e,0) xs
    | s == bestsize && e < bestent = x : better (s,e,0) xs
    | skips > 500000 = [] -- random give up point
    | otherwise = better (bestsize,bestent,skips+1) xs

run :: [Int] -> Int -> [(Int,Int)]
run nums bags = better (length buckets,1,0) $ map score sets
  where buckets = reverse $ sort nums
        groupsize = div (sum nums) bags
        sets = combos groupsize (replicate bags []) buckets

process :: [String] -> [String]
process rows = map (show . snd . last) [run nums 3, run nums 4]
    where nums = map read rows

main :: IO ()
main = interact (unlines . process . lines)
