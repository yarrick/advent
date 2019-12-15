import Intcode
import Data.Matrix
import Data.List

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

type Maze = (State, (Int,Int), Matrix Int)

posdiff :: (Int,Int) -> Int -> (Int,Int)
posdiff (x,y) 1 = (x,y+1)
posdiff (x,y) 2 = (x,y-1)
posdiff (x,y) 3 = (x-1,y)
posdiff (x,y) 4 = (x+1,y)

setm :: Matrix Int -> (Int,Int) -> Int -> Bool -> Matrix Int
setm m pos@(x,y) v realmove
  | prev >= 0 && realmove = setElem (prev+1) pos m
  | prev >= 0 && not realmove = m
  | otherwise = setElem v pos m
  where prev = getElem x y m

walk :: Maze -> Int -> Bool -> Maze
walk (state, pos, m) dir realmove
  | result == 2 = (newstate, posdiff pos dir, setElem (-9) (posdiff pos dir) m)
  | result == 1 && realmove = score (newstate, posdiff pos dir, setm m (posdiff pos dir) 0 realmove)
  | result == 1  = (newstate, posdiff pos dir, setm m (posdiff pos dir) 0 realmove)
  | result == 0 = (newstate, pos, setElem (-2) (posdiff pos dir) m)
  where newstate = exec $ (inputnum state $ toInteger dir) { outdata = [] }
        result = fromInteger $ head $ outdata newstate

getstate :: Maze -> Int -> Int
getstate (_,pos,m) dir = getElem x y m
  where (x,y) = posdiff pos dir

invdir :: Int -> Int
invdir 1 = 2
invdir 2 = 1
invdir 3 = 4
invdir 4 = 3

bump :: Maze -> Int -> Maze
bump maze@(st,pos,m) dir
  | dir == 5 = maze
  | getstate maze dir == 3 = bump maze (dir+1)
  | pos == pos2 = bump next (dir+1)
  | otherwise = bump (walk next (invdir dir) False) (dir+1)
  where next@(st2,pos2,m2) = walk maze dir False

score :: Maze -> Maze
score maze@(st,p,m)
  | length blocks == 3 && length unknowns == 0 = (st,p,setElem 10 p m)
  | otherwise = maze
  where states = map (getstate maze) [1,2,3,4]
        blocks = filter (\x -> x == -2 || x >= 10) states
        unknowns = filter (\x -> x == -1) states

explore :: Maze -> Int -> Maze
explore maze 0 = maze
explore maze@(st,(x,y),m) n
--  | length target > 0 = walk bumped (fst $ head target) False
  | otherwise = explore (walk bumped mindir True) (n-1)
  where bumped =  score $ bump (st,(x,y),setElem (getElem x y m) (x,y) m) 1
        target = filter (\(d,n) -> n == (-9)) $ zip [1..4] $ map (getstate bumped) [1,2,3,4]
        mindir = fst $ head $ sortBy (\(a,b) (c,d) -> compare b d) $ filter (\(d,n) -> n >= 0) $ zip [1..4] $ map (getstate bumped) [1,2,3,4]

mark :: Matrix a -> (Int,Int) -> a -> Matrix a
mark m (x,y) val = setElem val (x,y) m

mmark :: Matrix a -> a -> [(Int,Int)] -> Matrix a
mmark m _ [] = m
mmark m val (x:xs) = mmark (mark m x val) val xs

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem x y m

peers :: Matrix Int -> (Int,Int) -> Int -> [(Int,Int)]
peers m (x,y) val = map fst $ filter (\x -> snd x > val) neighbors
  where
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = zip cross (map (\pos -> getxy m pos (-1)) cross)

spread :: Matrix Int -> (Int,Int) -> Matrix Int
spread m pos
  | self == 999 = m
  | self < 0 = m
  | otherwise = mmark m (self+1) $ peers m pos self
  where self = getxy m pos (-1)

spreader :: Matrix Int -> [(Int,Int)] -> Matrix Int
spreader m [] = m
spreader m (p:ps) = spreader (spread m p) ps

allpos :: Matrix Int -> [(Int,Int)]
allpos m = concat $ map (\x -> zip (repeat x) c) r
  where r = [1..(nrows m)]
        c = [1..(ncols m)]

stepm :: Matrix Int -> Matrix Int
stepm m = spreader m $ allpos m

flow :: Matrix Int -> Matrix Int
flow m
  | m == newm = m
  | otherwise = flow newm
  where newm = stepm m

resetm :: Matrix Int -> (Int,Int) -> Matrix Int
resetm m (x,y)
  | getElem x y m >= 0 = setElem 999 (x,y) m
  | getElem x y m == (-9)= setElem 0 (x,y) m
  | otherwise = m

prepm :: Matrix Int -> Matrix Int
prepm m = foldl resetm m (allpos m)

run bytes = getElem 25 25 $ flow $ prepm endm
  where state = newhaltstate (parse bytes) [] needinput
        m = matrix 50 50 (\(i,j) -> -1)
        (endstate,(gx,gy),endm) = explore (state, (25,25), m) 1500

run2 bytes = maximum $ toList $ flow $ prepm endm
  where state = newhaltstate (parse bytes) [] needinput
        m = matrix 50 50 (\(i,j) -> -1)
        (endstate,(gx,gy),endm) = explore (state, (25,25), m) 1500
