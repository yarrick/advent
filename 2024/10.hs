import Data.List
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col

process (zeros, heights) = map (show.sum) [goals, map length paths]
    where paths = map (\p -> nub $ walk heights [p] 0) zeros
          goals = map (length.nub.map head) paths

walk :: M.Map Pos Int -> [Pos] -> Int -> [[Pos]]
walk _ p 9 = [p]
walk h ((r,c):ps) val = nub $ concatMap (\p -> walk h (p:(r,c):ps) (succ val)) nexts
    where neighbors = filter (\p -> M.member p h) [(r-1,c), (r,c-1), (r,c+1), (r+1,c)]
          nexts = filter (\p -> h M.! p == succ val) neighbors

parse :: [String] -> ([Pos], M.Map Pos Int)
parse ss = (map fst $ filter (\(p,v) -> v == 0) cells, M.fromList cells)
    where inject (r,cs) = map (\(c,v) -> ((r,c),read [v])) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) ss

main :: IO ()
main = interact (unlines . process . parse . lines)
