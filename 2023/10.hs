import Prelude hiding (Left, Right)
import Data.Matrix

data Dir = North | South | West | East deriving (Eq, Ord, Show)
data Inside = Left | Right deriving (Eq, Show)

process :: [String] -> [String]
process rows = map show [length path `div` 2 , length contained]
    where pipes = fromLists $ map (map parse) rows
          rcells (r,row) = [(r,c) | (c,s) <- zip [1..] row, s == 'S' ]
          start = head $ concatMap rcells $ zip [1..] rows
          paths = map (\d -> follow pipes start start d []) [North, South, West, East]
          path = head $ filter ([]/=) paths
          contained = filter ('*'==) $ toList $ inside pipes path

inside m path = flow $ foldl fill pm $ look inner (snd start) $ spin path
    where pm = matrix (nrows m) (ncols m) ispipe
          ispipe (r,c)
            | elem (r,c) $ map fst path = '#'
            | otherwise = 'o'
          start = minimum path
          inner
            | elem (snd start) [North, East] = Right
            | otherwise = Left
          spin (s:ss)
            | s == start = (s:ss)
            | otherwise = spin (ss ++ [s])

fill :: Matrix Char -> (Int, Int) -> Matrix Char
fill m pos@(r,c)
    | m ! pos /= 'o' = m
    | otherwise = setElem '*' pos m

look :: Inside -> Dir -> [((Int, Int), Dir)] -> [(Int, Int)]
look _ _ [] = []
look Left pdir (((r,c), dir):ps)
    | dir == North && pdir == West = (r+1,c) : (r+1,c-1) : (r,c-1) : look Left dir ps
    | dir == North = (r,c-1) : look Left dir ps
    | dir == South && pdir == East = (r-1,c) : (r-1,c+1) : (r,c+1) : look Left dir ps
    | dir == South = (r,c+1) : look Left dir ps
    | dir == West && pdir == South = (r,c+1) : (r+1,c+1) : (r+1,c) : look Left dir ps
    | dir == West = (r+1,c) : look Left dir ps
    | dir == East && pdir == North = (r,c-1) : (r-1,c-1) : (r-1,c) : look Left dir ps
    | dir == East = (r-1,c) : look Left dir ps
look Right pdir (((r,c), dir):ps)
    | dir == North && pdir == East = (r+1,c) : (r+1,c+1) : (r,c+1) : look Right dir ps
    | dir == North = (r,c+1) : look Right dir ps
    | dir == South && pdir == West = (r-1,c) : (r-1,c-1) : (r,c-1) : look Right dir ps
    | dir == South = (r,c-1) : look Right dir ps
    | dir == West && pdir == North = (r,c+1) : (r-1,c+1) : (r-1,c) : look Right dir ps
    | dir == West = (r-1,c) : look Right dir ps
    | dir == East && pdir == South = (r,c-1) : (r+1,c-1) : (r+1,c) : look Right dir ps
    | dir == East = (r+1,c) : look Right dir ps

flow :: Matrix Char -> Matrix Char
flow m
    | m == nextm = m
    | otherwise = flow nextm
    where nextm = mapPos (tick m) m

tick :: Matrix Char -> (Int, Int) -> Char -> Char
tick m pos@(rr,cc) ch
    | ch /= 'o' = ch
    | elem '*' cross = '*'
    | otherwise = ch
    where cross = [m ! (r,c) | r <- [rr-1,rr+1], c <- [cc-1,cc+1], r > 0, c > 0,
                               r <= nrows m, c <= ncols m]

follow :: Matrix [Dir] -> (Int, Int) -> (Int, Int) -> Dir -> [((Int, Int), Dir)] -> [((Int, Int), Dir)]
follow m goal pos dir path
    | newpos == goal = path ++ [(pos,dir)]
    | newdirs == [] = []
    | otherwise = follow m goal newpos (head newdirs) (path ++ [(pos,dir)])
    where newpos = next pos dir
          newdirs = step m dir newpos

step :: Matrix [Dir] -> Dir -> (Int, Int) -> [Dir]
step m dir (r,c)
    | r < 1 || c < 1 = []
    | r > nrows m || c > ncols m = []
    | elem opp dirs = filter (opp/=) dirs
    | otherwise = []
    where dirs = m ! (r,c)
          opp = opposite dir

opposite :: Dir -> Dir
opposite North = South
opposite South = North
opposite West = East
opposite East = West

next :: (Int, Int) -> Dir -> (Int, Int)
next (r,c) North = (r-1,c)
next (r,c) South = (r+1,c)
next (r,c) West = (r,c-1)
next (r,c) East = (r,c+1)

parse :: Char -> [Dir]
parse '|' = [North, South]
parse '-' = [West, East]
parse 'L' = [North, East]
parse 'J' = [North, West]
parse '7' = [South, West]
parse 'F' = [South, East]
parse _ = []

main :: IO ()
main = interact (unlines . process . lines)
