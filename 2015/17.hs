import Data.List

combos :: Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
combos _ a [] = a
combos limit prev (i:is) = combos limit (goodsize ++ (map (\(s,c) -> (s+i,c+1)) goodsize)) is
  where goodsize = filter (\(s,_) -> s <= limit) prev

run str limit = length $ filter (==limit) $ map fst $ combos limit [(0,0)] buckets
  where buckets = reverse $ sort $ map read $ words str

run2 str limit = length $ takeWhile (\(_,c) -> c==mincount) matches
  where buckets = reverse $ sort $ map read $ words str
        counts (_,a) (_,b) = compare a b
        matches = sortBy counts $ filter (\(s,_) -> s == limit) $ combos limit [(0,0)] buckets
        mincount = snd $ head matches
