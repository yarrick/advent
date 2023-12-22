import Data.Char
import Data.Matrix
import qualified Data.Map as M

process :: [String] -> [String]
process rows = [show $ solve 64]
    where rcells (r,row) = [(r,c) | (c,s) <- zip [1..] row, s == 'S' ]
          start = head $ concatMap rcells $ zip [1..] rows
          pathed = paths (fromLists rows) (M.fromList [(start,(90001,0))]) [start]
          solve n = length $ filter (\l -> l <= n) $ map (snd.snd) $ M.toList pathed

paths :: Matrix Char -> M.Map (Int,Int) (Int,Int) -> [(Int,Int)] -> M.Map (Int,Int) (Int,Int)
paths _ dists [] = dists
paths m dists ((rr,cc):ps) = paths m (foldl update dists bettered) (ps++map fst bettered)
    where reachable (r,c) = r >= 1 && r <= nrows m && c >= 1 && c <= ncols m && m ! (r,c) /= '#'
          lookup p = (p, M.findWithDefault (90001,90000) p dists)
          around = [ (r,cc) | r <- [rr-1,rr+1] ] ++ [(rr,c) |  c <- [cc-1,cc+1] ]
          nbrs = map lookup $ filter reachable around
          update dm (p,v) = M.insert p v dm
          bettered = better nbrs (dists M.! (rr,cc))

better :: [((Int,Int),(Int,Int))] -> (Int,Int) -> [((Int,Int),(Int,Int))]
better [] _ = []
better ((p,prev):ps) dist
    | next < prev = (p,next) : better ps dist
    | otherwise = better ps dist
    where next = (minimum [fst prev, 1 + snd dist], minimum [snd prev, 1 + fst dist])

main :: IO ()
main = interact (unlines . process . lines)
