import Prelude hiding (Left, Right)
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col
data Dir = Up | Right | Down | Left deriving (Eq,Show)
type Path = (Int, S.Set Pos) -- score and seen cells. In list in Dir order

process :: (Pos, Pos, M.Map Pos [Path]) -> [String]
process (start, end, openmaze) = [show score, show $ length cells]
    where maze = M.insert (start) (zip [1000, 0, 1000, 99999] $ cycle [S.singleton start]) openmaze
          (score,cells) = head $ sortBy smallest $ (paths maze $ S.singleton start) M.! end
          smallest (a,_) (b,_) = compare a b

paths :: M.Map Pos [Path] -> S.Set Pos -> M.Map Pos [Path]
paths maze cands
    | S.null cands = maze
    | otherwise = paths nmaze $ S.fromList $ rest ++ ncands
    where (cur:rest) = S.toList cands
          ps = zip [Up, Right, Down, Left] $ maze M.! cur
          reachable = map (\(dir,(score, seen)) -> (step cur dir,dir,(succ score, seen))) ps
          existing = filter (\(p,d,(sc,seen)) -> sc < 99999 && M.member p maze) reachable
          (nmaze, ncands) = expand maze [] existing

expand :: M.Map Pos [Path] -> [Pos] -> [(Pos, Dir, Path)] -> (M.Map Pos [Path], [Pos])
expand maze prev [] = (maze, prev)
expand maze prev ((pos, dir, (score, seen)):cs)
    | mergedpath /= oldpath = expand (M.insert pos mergedpath maze) (pos:prev) cs
    | otherwise = expand maze prev cs
    where newpath = zip (turns dir score) $ cycle [S.insert pos seen]
          oldpath = maze M.! pos
          closer ((newd,seen),(oldd,oldseen))
            | newd < oldd = (newd, seen)
            | newd == oldd = (newd, S.union seen oldseen)
            | otherwise = (oldd, oldseen)
          mergedpath = map closer $ zip newpath oldpath

step (r,c) Up = (r-1,c)
step (r,c) Right = (r,c+1)
step (r,c) Down = (r+1,c)
step (r,c) Left = (r,c-1)

turns Up dist = [dist, dist+1000, 99999, dist+1000]
turns Right dist = [dist+1000, dist, dist+1000, 99999]
turns Down dist = [99999, dist+1000, dist, dist+1000]
turns Left dist = [dist+1000, 99999, dist+1000, dist]

parse :: [String] -> (Pos, Pos, M.Map Pos [Path])
parse grid = (head $ select "S", head $ select "E",
              M.fromList $ zip (select ".E") (cycle [take 4 $ cycle [(99999,S.empty)]]))
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) grid
          select str = map fst $ filter (\(a,b) -> elem b str) cells

main :: IO ()
main = interact (unlines . process . parse . lines)
