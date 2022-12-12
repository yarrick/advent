import Data.Char
import Data.List
import Data.Matrix

process :: (Matrix Int, [(Char, (Int,Int))]) -> [String]
process (m, poss) = map show [cost start, minimum $ map cost $ cands m]
    where ((_,(er,ec)):(_,start):[]) = sort poss
          mcost st = setElem 0 st $ matrix (nrows m) (ncols m) (\_ -> 99999)
          cost st = getElem er ec $ snd $ flow (m, mcost st) (map fst $ neighbors m st)

cands :: (Matrix Int) ->[(Int, Int)]
cands m = filter hasb as
    where as = filter (\(r,c) -> getElem r c m == 1) $ cells m
          hasb p = length (filter (\((r,c),h) -> h == 2) $ neighbors m p) > 0

flow :: (Matrix Int, Matrix Int) -> [(Int, Int)] -> (Matrix Int, Matrix Int)
flow (mh, mc) [] = (mh, mc)
flow (mh, mc) (pos:pps)
    | changed = flow (mh, nc) (pps ++ (map fst $ neighbors mh pos))
    | otherwise = flow (mh,mc) pps
    where (_,nc,changed) = update (mh,mc) pos

neighbors :: Matrix Int -> (Int,Int) -> [((Int,Int), Int)]
neighbors m (rr,cc) = [((r,c), getElem r c m) | (r,c) <- [(rr-1,cc), (rr,cc-1), (rr+1,cc), (rr,cc+1)],
                       c >= 1 && c <= ncols m && r >= 1 && r <= nrows m ]

update :: (Matrix Int, Matrix Int) -> (Int, Int) -> (Matrix Int, Matrix Int, Bool)
update (mh, mc) (r,c)
    | costs == [] = (mh, mc, False)
    | mincost < mycost = (mh, setElem mincost (r,c) mc, True)
    | otherwise = (mh, mc, False)
    where mycost = getElem r c mc
          height = getElem r c mh
          nbrs = filter (\(p,h) -> h >= height - 1) $ neighbors mh (r,c)
          costs = sort $ map (\((r,c),_) -> (getElem r c mc, (r,c))) nbrs
          mincost = succ $ fst $ head costs

cells :: Matrix a -> [(Int,Int)]
cells m = [(r,c) | r <- [1..nrows m], c <- [1..ncols m] ]

parse :: [String] -> (Matrix Int, [(Char, (Int,Int))])
parse rows = (matrix (nrows mchar) (ncols mchar) val, filter (isUpper.fst) special)
    where mchar = fromLists rows
          special = map (\(r,c) -> (getElem r c mchar, (r,c))) $ cells mchar
          val (r,c)
            | char == 'S' = 1
            | char == 'E' = 26
            | otherwise = ord char - (ord 'a') + 1
            where char = getElem r c mchar

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

