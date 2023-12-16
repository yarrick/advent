import Data.Char
import Data.List
import Data.Matrix

parse :: String-> [(Char,[Int])]
parse ws = sort $ map (range.axr) $ words ws
    where axr p = (head p, filter (\x -> isDigit $ head x) $ groupBy (\a b -> isDigit a == isDigit b) $ drop 2 p)
          range (c,rs) = (c,[read (head rs)..read (last rs)])

build :: [(Int,[Int])] -> Int -> Int -> Matrix Char
build rpoints rows cols = matrix rows cols val
    where val (r,c)
            | elem c cols = '#'
            | otherwise = '.'
            where cols = concatMap snd $ filter (\(rr,cc) -> rr == r) rpoints

flow :: (Int,Int) -> Int -> Char -> Matrix Char -> Matrix Char
flow (r,c) dc ch m
    | bval /= '~' && bval /= '#' = m
    | val == '#' = m
    | val == '~' = flow (r,c+dc) dc ch m
    | otherwise = flow (r,c+dc) dc ch $ unsafeSet ch (r,c) m
    where val = unsafeGet r c m
          bval = unsafeGet (r+1) c m

flowable :: (Int,Int) -> Int -> Matrix Char -> Bool
flowable (r,c) dc m
    | bval /= '~' && bval /= '#' = False
    | val == '#' = True
    | otherwise = flowable (r,c+dc) dc m
    where val = unsafeGet r c m
          bval = unsafeGet (r+1) c m

-- new water drop positions
drops :: (Int,Int) -> Int -> Matrix Char -> [(Int,Int)]
drops (r,c) dc m
    | val == '#' = []
    | r < nrows m && bval /= '~' && bval /= '#' = [(r,c)]
    | otherwise = drops (r,c+dc) dc m
    where val = unsafeGet r c m
          bval = unsafeGet (r+1) c m

drip :: ([(Int,Int)], Matrix Char) -> ([(Int,Int)], Matrix Char)
drip ([], m) = ([], m)
drip ((wr,wc):ws, m)
    | wr > nrows m = drip (ws, m)
    | wr == nrows m = drip (ws, unsafeSet '|' (nr,wc) m)
    | nval == '.' = drip ((nr,wc):ws, unsafeSet '|' (nr,wc) m)
    | fill && elem nval "~#" = drip ((wr-1,wc):ws, flowrow '~')
    | elem nval "~#" = drip (ws ++ taps, foldl tap (flowrow '|') taps)
    | otherwise = drip (ws, m)
    where nr = wr+1
          nval = unsafeGet nr wc m
          fill = flowable (wr,wc) (-1) m && flowable (wr,wc) 1 m
          flowrow ch = flow (wr,wc) (-1) ch $ flow (wr,wc) 1 ch m
          taps = drops (wr,wc) (-1) m ++ drops (wr,wc) 1 m
          tap m pos = unsafeSet '|' pos m

process :: [[(Char,[Int])]] -> [String]
process input = map (show.length) [filter (\c -> elem c "~|") flowchart, filter (=='~') flowchart]
    where expand ((_,xs):(_,ys):zz) = [(r,c) | r <- ys, c <- xs ]
          rawpoints = concatMap expand input
          bounds r = (minimum r, maximum r)
          (minr,maxr) = bounds $ map fst rawpoints
          (minc,maxc) = bounds $ map snd rawpoints
          rowoffset = 1 - minr
          coloffset = 2 - minc
          points = sort $ map (\(r,c) -> (r+rowoffset, c+coloffset)) rawpoints
          rowpoints = map (\a -> (fst $ head a, map snd a)) $ groupBy (\(a,_) (b,_) -> a == b) points
          chart = build rowpoints (maxr+rowoffset) (maxc+coloffset+1)
          water = (0,500 + coloffset)
          flowchart = toList $ snd $ drip ([water],chart)

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
