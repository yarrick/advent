import Data.List

process :: [String] -> [String]
process rows = map (show.sum.paths.expand) [2, 1000000]
    where gcells (r,row) = [(r,c) | (c,s) <- zip [1..] row, s == '#' ]
          gs = concatMap gcells $ zip [1..] rows
          flip (a,b) = (b,a)
          expand n = grow n (1,1) $ map (\(a,b) -> (b,a)) $ grow n (1,1) gs

paths :: [(Int,Int)] -> [Int]
paths (g:[]) = []
paths (g:gs) = map (dist g) gs ++ paths gs
    where dist (ar,ac) (br,bc) = abs (ar-br) + abs (ac-bc)

grow :: Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
grow _ _ [] = []
grow width (from,to) gs
    | match == [] = grow width (succ from, to + width) gs
    | otherwise = map move match ++ grow width (succ from, succ to) rest
    where (match,rest) = partition (\a -> fst a == from) gs
          move (r,c) = (to,c)

main :: IO ()
main = interact (unlines . process . lines)
