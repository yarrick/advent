import Data.Char
import Data.List
import qualified Data.Map as M

parse :: String -> [(Char,[Int])]
parse ws = sort $ map (range.axr) $ words ws
    where axr p = (head p, filter (\x -> isDigit $ head x) $ groupBy (\a b -> isDigit a == isDigit b) $ drop 2 p)
          range (c,rs) = (c,[read (head rs)..read (last rs)])

flow :: (Int,Int) -> Int -> Char -> M.Map (Int,Int) Char -> M.Map (Int,Int) Char
flow (r,c) dc ch m
    | bval /= '~' && bval /= '#' = m
    | val == '#' = m
    | val == '~' = flow (r,c+dc) dc ch m
    | otherwise = flow (r,c+dc) dc ch $ M.insert (r,c) ch m
    where val = M.findWithDefault '.' (r,c) m
          bval = M.findWithDefault '.' (r+1,c) m

flowable :: (Int,Int) -> Int -> M.Map (Int,Int) Char -> Bool
flowable (r,c) dc m
    | bval /= '~' && bval /= '#' = False
    | val == '#' = True
    | otherwise = flowable (r,c+dc) dc m
    where val = M.findWithDefault '.' (r,c) m
          bval = M.findWithDefault '.' (r+1,c) m

-- new water drop positions
drops :: (Int,Int) -> Int -> Int -> M.Map (Int,Int) Char -> [(Int,Int)]
drops (r,c) maxr dc m
    | val == '#' = []
    | r < maxr && notElem bval "~#" = [(r,c)]
    | otherwise = drops (r,c+dc) maxr dc m
    where val = M.findWithDefault '.' (r,c) m
          bval = M.findWithDefault '.' (r+1,c) m

drip :: ([(Int,Int)], Int, M.Map (Int,Int) Char) -> M.Map (Int,Int) Char
drip ([], n, m) = m
drip ((wr,wc):ws, maxr, m)
    | wr >= maxr = drip (ws, maxr, m)
    | nval == '.' = drip ((nr,wc):ws, maxr, tap m (nr,wc))
    | fill && elem nval "~#" = drip ((wr-1,wc):ws, maxr, flowrow '~')
    | elem nval "~#" = drip (ws ++ taps, maxr, foldl tap (flowrow '|') taps)
    | otherwise = drip (ws, maxr, m)
    where nr = wr+1
          nval = M.findWithDefault '.' (nr,wc) m
          fill = flowable (wr,wc) (-1) m && flowable (wr,wc) 1 m
          flowrow ch = flow (wr,wc) (-1) ch $ flow (wr,wc) 1 ch m
          taps = drops (wr,wc) maxr (-1) m ++ drops (wr,wc) maxr 1 m
          tap m pos = M.insert pos '|' m

process :: [[(Char,[Int])]] -> [String]
process input = map (show.length) [filter (\c -> elem c "~|") flown, filter (=='~') flown]
    where expand ((_,xs):(_,ys):zz) = [(r,c) | r <- ys, c <- xs ]
          rawpoints = concatMap expand input
          minr = minimum $ map fst rawpoints
          maxr = maximum $ map fst rawpoints
          flowchart = drip ([(0,500)], maxr, M.fromList $ zip rawpoints (cycle "#"))
          flown = map snd $ filter (\((r,c),v) -> r >= minr && r <= maxr) $ M.toList flowchart

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
