import Data.List

combos :: Int -> [(Int,Int)] -> [Int] -> [(Int,Int)]
combos _ a [] = a
combos limit prev (i:is) = combos limit (goodsize ++ (map (\(s,c) -> (s+i,c+1)) goodsize)) is
  where goodsize = filter (\(s,_) -> s <= limit) prev

run buckets limit = length $ filter (==limit) $ map fst $ combos limit [(0,0)] buckets

run2 buckets limit = length $ takeWhile (\(_,c) -> c==mincount) matches
  where counts (_,a) (_,b) = compare a b
        matches = sortBy counts $ filter (\(s,_) -> s == limit) $ combos limit [(0,0)] buckets
        mincount = snd $ head matches

process str = map show [run buckets 150, run2 buckets 150]
    where buckets = reverse $ sort $ map read $ words str

main :: IO ()
main = interact (unlines . process)
