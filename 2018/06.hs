import Data.Matrix
import Data.List

parse :: String -> (Int,Int)
parse str = (y,x)
  where (x,y) = read $ "(" ++ str ++ ")"

sizing :: ([Int] -> Int) -> [(Int,Int)] -> (Int,Int)
sizing fun ps = (fun $ map fst ps, fun $ map snd ps)

data Direction = NW | N | NE | E | SE | S | SW | W | All deriving (Eq,Show)

next :: (Direction,Int,(Int,(Int,Int))) -> [(Direction,Int,(Int,(Int,Int)))]
next (N,d,(pid,(r,c))) = [(N,d+1,(pid,(r-1,c)))]
next (E,d,(pid,(r,c))) = [(E,d+1,(pid,(r,c+1)))]
next (S,d,(pid,(r,c))) = [(S,d+1,(pid,(r+1,c)))]
next (W,d,(pid,(r,c))) = [(W,d+1,(pid,(r,c-1)))]
next (NW,d,p@(pid,(r,c))) = (NW,d+2,(pid,(r-1,c-1))) : next (N,d,p) ++ next (W,d,p)
next (NE,d,p@(pid,(r,c))) = (NE,d+2,(pid,(r-1,c+1))) : next (N,d,p) ++ next (E,d,p)
next (SE,d,p@(pid,(r,c))) = (SE,d+2,(pid,(r+1,c+1))) : next (S,d,p) ++ next (E,d,p)
next (SW,d,p@(pid,(r,c))) = (SW,d+2,(pid,(r+1,c-1))) : next (S,d,p) ++ next (W,d,p)
next (All,d,p) = nub $ next (NW,d,p) ++ next (NE,d,p) ++ next (SE,d,p) ++ next (SW,d,p)

flow :: Matrix ([Int],Int) -> [(Direction,Int,(Int,(Int,Int)))] -> Matrix ([Int],Int)
flow m [] = m
flow m (move@(dir,dist,(pid,(r,c))):ss)
  | r < 1 || r > nrows m = flow m ss
  | c < 1 || c > ncols m = flow m ss
  | dist < celldist = flow (setElem ([pid],dist) (r,c) m) (ss++next move)
  | dist == celldist = flow (setElem (pid:ids,dist) (r,c) m) (ss++next move)
  | otherwise = flow m ss
  where (ids,celldist) = getElem r c m

sumcells :: [Int] -> [([Int],Int)] -> [Int]
sumcells c [] = c
sumcells counts (((owner:[]),_):os) = sumcells (take owner counts ++ [counts!!owner + 1] ++ drop (owner+1) counts) os
sumcells counts (o:os) = sumcells counts os

edgecells :: [[([Int],Int)]] -> [([Int],Int)]
edgecells rows = concat $ head rows : last rows : (map (\r -> [head r,last r]) $ take (length rows-2) $ tail rows)

totdist :: (Int,Int) -> [(Int,(Int,Int))] -> Int
totdist (r,c) pos = sum $ map dist pos
  where dist (_,(rr,cc)) = abs(r-rr) + abs(c-cc)

process rows = map show [last $ sort $ map snd $ filter (\(cell,_) -> not $ elem cell edgefields) cellcounts,
                 length $ filter (<10000) $ [ totdist (r,c) pos | r <- [vmin..vmax], c <- [hmin..hmax] ] ]
  where pos = zip [0..] $ map parse rows
        (vmax,hmax) = sizing maximum $ map snd pos
        (vmin,hmin) = sizing minimum $ map snd pos
        m = flow (matrix vmax hmax (\_ -> ([],9999))) $ map (\p -> (All,0,p)) pos
        mrows = toLists m
        cellcounts = zip [0..] $ foldl sumcells (take (length pos) $ repeat 0) mrows
        edgefields = nub $ sort $ concat $ filter (\a -> length a == 1) $ map fst $ edgecells mrows

main :: IO ()
main = interact (unlines . process . lines)
