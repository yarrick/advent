import Data.List
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col

process (start, end, track) = map (show.cheat) [c1, c2]
    where trackmap = M.fromList $ zip (start:end:track) (cycle [999999])
          [smap,emap] = map (\p -> paths (M.insert p 0 trackmap) [p]) [start,end]
          c1 (x,y) = [(x,y-2), (x-2,y), (x+2,y), (x,y+2)]
          c2 (ax,ay) = [(x,y) | x <- [ax-20..ax+20], y <- [ay-20..ay+20], jumpdist (x,y) (ax,ay) <= 20]
          cheat fn = length $ filter (>=100) $ cheats fn smap emap (smap M.! end) (M.keys smap)

cheats :: (Pos -> [Pos]) -> M.Map Pos Int -> M.Map Pos Int -> Int -> [Pos] -> [Int]
cheats _ sm em _ [] = []
cheats jmp sm em basedist ((x,y):ps) = filter (>0) gains ++ cheats jmp sm em basedist ps
    where jumps = filter (\p -> M.member p em) $ jmp (x,y)
          gains = map (\p -> basedist - (sm M.! (x,y)) - (em M.! p) - jumpdist p (x,y)) jumps

jumpdist (xx,yy) (x,y) = abs (xx-x) + abs (yy-y)

paths :: M.Map Pos Int -> [Pos] -> M.Map Pos Int
paths m [] = m
paths m ((x,y):bs) = paths newm $ (bs++closer)
    where neighbors = filter (\p -> M.member p m) [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]
          newdist = succ $ m M.! (x,y)
          closer = map fst $ filter (\(p,d) -> d > newdist) $ map (\n -> (n,m M.!n)) neighbors
          newm = foldr (\p mm -> M.insert p newdist mm) m closer

parse :: [String] -> (Pos, Pos, [Pos])
parse grid = (head $ select 'S', head $ select 'E', select '.')
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) grid
          select ch = map fst $ filter (\((a,b),v) -> v == ch) cells

main :: IO ()
main = interact (unlines . process . parse . lines)
