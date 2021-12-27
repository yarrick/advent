import Data.List
import qualified Data.Map as M

data Dir = East | South deriving (Eq,Ord,Show)
type Pos = (Int,Int)
type Board = (Int,Int,M.Map Pos Dir,M.Map Pos Dir)

parse :: [String] -> (Int,Int,[(Pos,Dir)])
parse rows = (length cells, length (head cells), concatMap conv $ concat cells)
    where cells = map (\(r,row) -> (zip (zip (repeat r) [0..]) row)) $ zip [0..] rows
          conv (pos,'>') = [(pos,East)]
          conv (pos,'v') = [(pos,South)]
          conv _ = []

move :: Board -> (Pos,Dir) -> (Pos,Dir)
move (rows,cols,emap,smap) ((r,c),dir)
    | all (M.notMember npos) [emap,smap] = (npos,dir)
    | otherwise = ((r,c),dir)
    where npos
            | dir == East = (r,mod (succ c) cols)
            | otherwise = (mod (succ r) rows,c)

step :: Board -> Board
step (rows,cols,emap,smap) = (rows,cols,emoved, moved emoved smap smap)
    where moved e s m = M.fromList $ map (move (rows,cols,e,s)) $ M.toList m
          emoved = moved emap smap emap

flow :: Int -> Board -> Int
flow n b@(_,_,em,sm)
    | meq em nem && meq sm nsm = succ n
    | otherwise = flow (succ n) nb
    where nb@(_,_,nem,nsm) = step b
          meq a b = sort (M.toList a) == sort (M.toList b)

process :: (Int,Int,[((Int,Int),Dir)]) -> [String]
process (rows,cols,scs) = [show $ flow 0 (rows,cols,dirmap East,dirmap South)]
    where dirmap dir = M.fromList $ filter (\(_,t) -> t == dir) scs

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

