import Prelude hiding (Left,Right)
import Data.List
import Data.Matrix hiding (trace)
import qualified Data.Map as M

data Dir = Up | Right | Down | Left deriving (Eq, Ord, Show)
type Pos = (Int, Int)
type ResMap = M.Map Pos [([Dir], Int)]

process :: Matrix Int -> [String]
process m = map show [flowed run1, flowed run2]
    where paths = M.fromList [((1,1),[([],0)])]
          flowed (a,b,c) = stored $ expand m (a,b,c) (nrows m,ncols m) (paths, [(1,1)], 99999999)
          run1 = (turn1, compact1, (\c -> True))
          run2 = (turn2, compact2, (\(a,_) -> length a >= 4))
          stored (_,_,c) = c

expand :: Matrix Int -> ((Pos, Int, Dir) -> ([Dir], Int) -> [(Pos, ([Dir], Int))],
                         [([Dir], Int)] -> [([Dir], Int)], ([Dir], Int) -> Bool) ->
          (Int,Int) -> (ResMap, [Pos], Int) -> (ResMap, [Pos], Int)
expand _ _ _ (a,[],b) = (a,[],b)
expand m (turn,comp,wins) target (a,pos:ps,best) = expand m (turn,comp,wins) target (nm, sortBy shorter pps, bs)
    where cands = concat [ turn n prev | n <- next m pos, prev <- a M.! pos ]
          saves = map (\(a,b) -> (a, regroup b)) $ regroup $ filter (\(_,(_,c)) -> c < best) cands
          (nm,pps,bs) = foldl (update (comp,wins) target) (a,ps,best) saves
          shorter a b = compare (minimum $ map snd (nm M.! a)) (minimum $ map snd (nm M.! b))

turn1 :: (Pos, Int, Dir) -> ([Dir], Int) -> [(Pos, ([Dir], Int))]
turn1 (np,loss,dir) (dirs, cost)
    | elem dir dirs && length dirs == 3 = []
    | elem dir dirs = [(np, (dir:dirs, cost+loss))]
    | elem (opposite dir) dirs = []
    | otherwise = [(np, ([dir], cost+loss))]

turn2 :: (Pos, Int, Dir) -> ([Dir], Int) -> [(Pos, ([Dir], Int))]
turn2 (np,loss,dir) (dirs, cost)
    | elem dir dirs && length dirs == 10 = []
    | elem dir dirs = [(np, (dir:dirs, cost+loss))]
    | elem (opposite dir) dirs = []
    | length dirs > 0 && length dirs < 4 = []
    | otherwise = [(np, ([dir], cost+loss))]

update :: ([([Dir], Int)] -> [([Dir], Int)], ([Dir], Int) -> Bool) -> (Int,Int) ->
          (ResMap, [Pos], Int) -> (Pos, [([Dir], [Int])]) -> (ResMap, [Pos], Int)
update (comp,wins) target (mp, updated, best) (pos, paths)
    | improved == prevpaths = (mp, updated, best)
    | pos == target && length winners > 0 = (M.insert pos improved mp, pos:updated, minimum $ best : winners)
    | otherwise = (M.insert pos improved mp, pos:updated, best)
    where prevpaths = sort $ M.findWithDefault [] pos mp
          improved = comp $ sort $ improve prevpaths paths
          winners = map snd $ filter wins improved

compact1 :: [([Dir], Int)] -> [([Dir], Int)]
compact1 (a:[]) = (a:[])
compact1 ((a,la):(b,lb):cs)
    | a == [] = (a,la) : compact1 ((b,lb):cs)
    | head a == head b && la <= lb = compact1 ((a,la):cs)
    | otherwise = (a,la) : compact1 ((b,lb):cs)

compact2 :: [([Dir], Int)] -> [([Dir], Int)]
compact2 (a:[]) = (a:[])
compact2 ((a,la):(b,lb):cs)
    | a == [] = (a,la) : compact2 ((b,lb):cs)
    | head a == head b && length a >= 4 && la <= lb = compact2 ((a,la):cs)
    | otherwise = (a,la) : compact2 ((b,lb):cs)

improve :: [([Dir], Int)] -> [([Dir], [Int])] -> [([Dir], Int)]
improve a [] = a
improve [] ((v,vals):vs) = (v,minimum vals) : improve [] vs
improve ((a,v):as) ((b,vs):bs)
    | a == b = (a,minimum (v:vs)) : improve as bs
    | otherwise = (a,v) : improve as ((b,vs):bs)

regroup :: (Eq c, Ord c, Ord b) => [(c,b)] -> [(c,[b])]
regroup v = map (\a -> (fst $ head a, map snd a)) $ groupBy (\a b -> fst a == fst b) $ sort v

opposite :: Dir -> Dir
opposite Up = Down
opposite Down = Up
opposite Right = Left
opposite Left = Right

next :: Matrix Int -> Pos -> [(Pos, Int, Dir)]
next m (r,c) = map (\(pos, dir) -> (pos, m ! pos, dir)) valid
    where cands = [((r-1,c),Up), ((r+1,c), Down), ((r,c-1), Left), ((r,c+1), Right)]
          valid = filter (\((r,c),_) -> r >= 1 && r <= nrows m && c >= 1 && c <= ncols m) cands

parse :: [String] -> Matrix Int
parse rows = fromLists nums
    where nums = map (map (\c -> read [c])) rows

main :: IO ()
main = interact (unlines . process . parse . lines)
