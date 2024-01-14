import Data.Char
import Data.List
import Data.Matrix
import qualified Data.Map as M
import qualified Data.Vector as V

-- board, distance to edge, steps to each cell, reached cells per step, edge cells
type Straight = (Matrix Char, [((Int,Int),Int)], M.Map (Int,Int) Int, V.Vector Int, [((Int,Int),(Int,Int))])

process :: [String] -> [String]
process rows = map (show.sum.reach) [64, 26501365]
    where rcells (r,row) = [(r,c) | (c,s) <- zip [1..] row, s == 'S' ]
          start = head $ concatMap rcells $ zip [1..] rows
          m = fromLists rows
          pathed = paths m (M.fromList [(start,0)]) [start]
          straights = map (\f -> genpath (m, [], pathed, V.empty, f $ nrows m)) [north,west,east,south]
          corners = map (\(p) -> (p, pathed M.! p)) [(1,1), (1,nrows m), (nrows m,1), (nrows m, nrows m)]
          diags = map (\(p,v) -> (succ v, precalc $ paths m (M.fromList [(p,1)]) [p])) corners
          reach n = (fetch (precalc pathed) n) : (map (crosswalk n) straights) ++
                    map (\(dist,v) -> diagwalk (n - dist) (nrows m) 1 v) diags

diagwalk :: Int -> Int -> Int -> V.Vector Int -> Int
diagwalk steps n lvl pv
    | found > 0 = found + diagwalk (steps-n) n (succ lvl) pv
    | otherwise = 0
    where found = lvl * (fetch pv steps)

-- position in old, position in new
north n = [((1,c),(n,c)) | c <- [1..n] ]
west n  = [((r,1),(r,n)) | r <- [1..n] ]
east n  = [((r,n),(r,1)) | r <- [1..n] ]
south n = [((n,c),(1,c)) | c <- [1..n] ]

precalc :: M.Map a Int -> V.Vector Int
precalc m = V.fromList $ steps 0 0 0 $ sort $ M.elems m
    where steps o e n []
            | odd n = [o]
            | otherwise = [e]
          steps o e n (s:ss)
            | s > n && odd n = o : steps o e (n+1) (s:ss)
            | s > n = e : steps o e (n+1) (s:ss)
            | odd n = steps (o+1) e n ss
            | otherwise = steps o (e+1) n ss

fetch :: V.Vector Int -> Int -> Int
fetch v n
    | n < 0 = 0
    | n < V.length v = v V.! n
    | odd n == odd maxn = v V.! maxn
    | otherwise = v V.! (pred maxn)
    where maxn = V.length v - 1

crosswalk :: Int -> [Straight] -> Int
crosswalk steps ((c@(_,ds,_,pv,_)):cs)
    | newsteps < 0 = 0
    | cs == [] = found + crosswalk newsteps (c:cs)
    | otherwise = found + crosswalk newsteps cs
    where newsteps = steps - minimum (map snd ds)
          found = fetch pv newsteps

genpath :: Straight -> [Straight]
genpath (m,_,prevpath,_,edge)
    | startdist pathed edge == mds = [res]
    | otherwise = res : genpath res
    where startdist m ps = map (\(p,np) -> (np, m M.! p)) ps
          mds = startdist prevpath edge
          delta = minimum (map snd mds) - 1
          pathed = paths m (M.fromList $ map (\(p,v) -> (p,v-delta)) mds) (map fst mds)
          res = (m, mds, pathed, precalc pathed, edge)

paths :: Matrix Char -> M.Map (Int,Int) Int -> [(Int,Int)] -> M.Map (Int,Int) Int
paths m dists [] = dists
paths m dists ((rr,cc):ps) = paths m (foldl update dists newnbrs) (ps++newnbrs)
    where reachable (r,c) = r >= 1 && r <= nrows m && c >= 1 && c <= ncols m && m ! (r,c) /= '#'
          around = [ (r,cc) | r <- [rr-1,rr+1] ] ++ [(rr,c) |  c <- [cc-1,cc+1] ]
          nbrs = map (\p -> (p, M.findWithDefault 9999999 p dists)) $ filter reachable around
          next = succ $ dists M.! (rr,cc)
          newnbrs = map fst $ filter (\(p,v) -> v > next) nbrs
          update dm p = M.insert p next dm

main :: IO ()
main = interact (unlines . process . lines)
