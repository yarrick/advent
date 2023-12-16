import Data.Matrix
import Data.List

generate :: [String] -> Matrix Int
generate input = matrix (length input) (length $ head input) (fill input)

fill :: [String] -> (Int,Int) -> Int
fill input (i,j)
  | c == '#' = -1
  | otherwise = 999999
  where c = (input !! (i-1)) !! (j - 1)

mark :: Matrix a -> (Int,Int) -> a -> Matrix a
mark m (x,y) val = setElem val (x+1,y+1) m

mmark :: Matrix a -> a -> [(Int,Int)] -> Matrix a
mmark m _ [] = m
mmark m val (x:xs) = mmark (mark m x val) val xs

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 0 = fallback
  | y < 0 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem (x+1) (y+1) m

peers :: Matrix Int -> (Int,Int) -> Int -> [(Int,Int)]
peers m (x,y) val = map fst $ filter (\x -> snd x > val) neighbors
  where
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = zip cross (map (\pos -> getxy m pos (-1)) cross)

spread :: Matrix Int -> (Int,Int) -> Matrix Int
spread m pos
  | self == 999999 = m
  | self == -1 = m
  | otherwise = mmark m (self+1) $ peers m pos self
  where self = getxy m pos (-1)

spreader :: Matrix Int -> [(Int,Int)] -> Matrix Int
spreader m [] = m
spreader m (p:ps) = spreader (spread m p) ps

allpos :: Matrix Int -> [(Int,Int)]
allpos m = concat $ map (\x -> zip (repeat x) c) r
  where r = [0..(nrows m)]
        c = [0..(ncols m)]

step :: Matrix Int -> Matrix Int
step m = spreader m $ allpos m

flow :: Matrix Int -> Matrix Int
flow m
  | m == newm = m
  | otherwise = flow newm
  where newm = step m

targets :: [String] -> [(Int,(Int,Int))]
targets input = map (\(x,(y,c)) -> (read [c],(x,y))) $ filter (\(_,(_,c)) -> c >= '0' && c <= '9') $
  concat $ map (\(a,bs) -> zip (repeat a) bs) $ zip [0..] $ map (\row -> zip [0..] row) input

getdist :: (Int,(Int,Int)) -> [(Int,(Int,Int))] -> Matrix Int -> [((Int,Int),Int)]
getdist (idx,pos) targets m = map (\t -> dist idx t) targets
  where mm = flow $ mark m pos 0
        dist from (to,loc) = ((from,to),getxy mm loc 0)

walk :: [((Int,Int),Int)] -> [Int] -> Int
walk _ (a:[]) = 0
walk dists (from:to:xs) = dist (from,to) + walk dists (to:xs)
  where dist p = snd $ head $ filter (\(a,_) -> a == p) dists

run (dists,paths) = minimum $ map (walk dists) paths

run2 (dists,paths) = minimum $ map (walk dists) $ map (\p -> p ++ [0]) paths

process :: [String] -> [String]
process rows = map show [run (dists,paths), run2 (dists, paths)]
  where m = generate rows
        trg = sort $ targets rows
        dists = concat $ map (\t -> getdist t trg m) trg
        paths = map (0:) $ permutations $ tail $  map fst $ trg


main :: IO ()
main = interact (unlines . process . lines)
