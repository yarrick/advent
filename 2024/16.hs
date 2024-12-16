import Prelude hiding (Left, Right)
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col
data Dir = Up | Right | Down | Left deriving (Eq,Ord,Show)

process :: (Pos, Pos, S.Set Pos) -> [String]
process (start, end, walls) = [show score, show $ S.size paths]
    where (score,_,paths) = head $ solve walls end [(0, (start, Right), S.singleton start)]

solve :: S.Set Pos -> Pos -> [(Int, (Pos, Dir), S.Set Pos)] -> [(Int, (Pos, Dir), S.Set Pos)]
solve walls _ [] = []
solve walls end (a@(sc,(p,d),ps):bs)
    | p == end = (sc,(p,d),ps) : solve walls end bs
    | otherwise = solve walls end $ prep $ nexts ++ (turns walls a) ++ bs
    where nexts = filter (\a -> S.notMember (step p d) walls) [(succ sc,((step p d),d),S.insert (step p d) ps)]

prep :: [(Int, (Pos, Dir), S.Set Pos)] -> [(Int, (Pos, Dir), S.Set Pos)]
prep ps = sortBy dist $ map (\(a,(b,c)) -> (b,a,c)) $ M.toList m
    where merger k (new,ps) (old,pps)
            | new == old = (new, S.union ps pps)
            | new < old = (new, ps)
            | otherwise = (old, pps)
          m = foldl (\m (sc,pd,ps) -> M.insertWithKey merger pd (sc,ps) m) M.empty ps
          dist (a,_,_) (b,_,_) = compare a b

turns :: S.Set Pos -> (Int, (Pos, Dir), S.Set Pos) -> [(Int,(Pos,Dir), S.Set Pos)]
turns walls (sc,(p,d),ps)
    | elem d [Left,Right] = map (\dd -> (sc+1000,(p,dd),ps)) $ filter usable [Up,Down]
    | otherwise = map (\dd -> (sc+1000,(p,dd),ps)) $ filter usable [Left,Right]
    where usable ds = S.notMember (step p ds) walls

step (r,c) Up = (r-1,c)
step (r,c) Right = (r,c+1)
step (r,c) Down = (r+1,c)
step (r,c) Left = (r,c-1)

parse :: [String] -> (Pos, Pos, S.Set Pos)
parse grid = (head $ select 'S', head $ select 'E', S.fromList $ select '#')
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) grid
          select ch = map fst $ filter (\(a,b) -> b == ch) cells

main :: IO ()
main = interact (unlines . process . parse . lines)
