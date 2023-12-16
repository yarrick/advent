import Prelude hiding (Left, Right)
import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe

data Dir = Up | Right | Down | Left deriving (Enum, Eq, Show)

type Tile = ((Int, Int), Matrix Char)

-- tile row/col, matrix row/col, direction
type Position = ((Int, Int), (Int, Int), Dir)

tilesize :: [String] -> Int
tilesize rs = minimum $ map length $ group $ map lens rs
    where lens r = map length [takeWhile isSpace r, dropWhile isSpace r]

tiles :: Int -> Int -> [String] -> [Tile]
tiles _ _ [] = []
tiles n r rs = (filter good $ rc $ map fromLists $ tchunk $ map xtile (take n rs))
    ++ tiles n (succ r) (drop n rs)
    where xtile [] = []
          xtile cs = (take n cs) : xtile (drop n cs)
          tchunk ([]:_) = []
          tchunk ts = (map head ts) : tchunk (map tail ts)
          rc ms = zip (zip (repeat r) [0..]) ms
          good (_,m) = not $ isSpace $ getElem 1 1 m

turn :: Dir -> Char -> Dir
turn Left 'R' = Up
turn d 'R' = succ d
turn Up 'L' = Left
turn d 'L' = pred d

next :: (Int, Int) -> Dir -> (Int, Int)
next (r,c) Up = (r-1,c)
next (r,c) Right = (r,c+1)
next (r,c) Down = (r+1,c)
next (r,c) Left = (r,c-1)

tile :: [Tile] -> (Int, Int) -> Maybe Tile
tile ts tp
    | length t == 0 = Nothing
    | otherwise = Just $ head t
    where t = filter ((==tp).fst) ts

furthest :: [Tile] -> (Int,Int) -> Dir -> (Int,Int)
furthest ts pos dir
    | isNothing nt = pos
    | otherwise = furthest ts (fst $ fromJust nt) dir
    where nt = tile ts $ next pos dir

ntile :: [Tile] -> (Int,Int) -> Dir -> (Int,Int)
ntile ts tp dir
    | isJust nt = fst $ fromJust nt
    | otherwise = furthest ts tp oppdir
    where nt = tile ts $ next tp dir
          oppdir = turn (turn dir 'R') 'R'

npos1 :: [Tile] -> Position -> Position
npos1 ts (t, pos, dir)
    | c < 1 = (ntile ts t dir, (r,tsize), dir)
    | c > tsize = (ntile ts t dir, (r,1), dir)
    | r < 1 = (ntile ts t dir, (tsize,c), dir)
    | r > tsize = (ntile ts t dir, (1,c), dir)
    | otherwise = (t, (r,c), dir)
    where (r,c) = next pos dir
          tsize = ncols $ snd $ fromJust $ tile ts t

walk :: [Tile] -> ([Tile] -> Position -> Position) -> Position -> Int -> Position
walk _ _ p 0 = p
walk ts func p len
    | getElem r c m == '#' = p
    | otherwise = walk ts func np (pred len)
    where np@(nt, (r,c), _) = func ts p
          (_,m) = fromJust $ tile ts nt

follow :: [Tile] -> ([Tile] -> Position -> Position) -> Position -> String -> Position
follow ts func p@(t, pos, dir) cmd
    | isAlpha (head cmd) = (t, pos, foldl turn dir cmd)
    | otherwise = walk ts func p (read cmd)

score :: [Tile] -> Position -> Int
score ts ((tr,tc), (r,c), dir) = ((tsize*tr+r)*1000 + ((tsize*tc)+c)*4 + td dir)
    where tsize = ncols $ snd $ head ts
          td Right = 0
          td Down = 1
          td Left = 2
          td Up = 3

check ts d (tr,tc)
    | isJust spot = (fst $ fromJust spot, 0)
    | d == Right && isJust north1 = (fst $ fromJust north1, 90)
    | d == Right && isJust south1 = (fst $ fromJust south1, 270)
    | d == Up && isJust east1 = (fst $ fromJust east1, 270)
    | d == Up && isJust west1 = (fst $ fromJust west1, 90)
    | d == Down && isJust east1 = (fst $ fromJust east1, 90)
    | d == Down && isJust west1 = (fst $ fromJust west1, 270)
    | d == Left && isJust north1 = (fst $ fromJust north1, 270)
    | d == Left && isJust south1 = (fst $ fromJust south1, 90)
    | elem d [Left,Right] && isJust north2 = (fst $ fromJust north2, 180)
    | elem d [Up,Down] && isJust west2 = (fst $ fromJust west2, 180)
    | elem d [Up,Down] && isJust east2 = (fst $ fromJust east2, 180)
    | elem d [Left,Right] && isJust south2 = (fst $ fromJust south2, 180)
    | d == Up && isJust south1 && isJust south2 && isJust southwest22 =
            (fst $ fromJust southwest22, 180)
    | d == Up && isJust south1 && isJust south2 && isJust southeast22 =
            (fst $ fromJust southeast22, 180)
    | d == Down && isJust north1 && isJust north2 && isJust northwest22 =
            (fst $ fromJust northwest22, 180)
    | d == Down && isJust north1 && isJust north2 && isJust northeast22 =
            (fst $ fromJust northeast22, 180)
    | d == Right && isJust west1 && isJust west2 && isJust southwest22 =
            (fst $ fromJust southwest22, 180)
    | d == Right && isJust west1 && isJust west2 && isJust northwest22 =
            (fst $ fromJust northwest22, 180)
    | d == Left && isJust east1 && isJust east2 && isJust southeast22 =
            (fst $ fromJust southeast22, 180)
    | d == Left && isJust east1 && isJust east2 && isJust northeast22 =
            (fst $ fromJust northeast22, 180)
    | d == Up && isJust south1 && isJust south2 && isJust southwest41 =
            (fst $ fromJust southwest41, 270)
    | d == Left && isJust east1 && isJust east2 && isJust southeast14 =
            (fst $ fromJust southeast14, 270)
    | d == Up && isJust south1 && isJust southwest42 =
            (fst $ fromJust southwest42, 0)
    | d == Down && isJust north1 && isJust northwest23 =
            (fst $ fromJust northwest23, 90)
    | d == Left && isJust east1 && isJust northeast32 =
            (fst $ fromJust northeast32, 90)
    | d == Down && isJust north1 && isJust north2 && isJust northeast42 =
            (fst $ fromJust northeast42, 0)
    where spot = tile ts (tr,tc)
          east1 = tile ts (tr,tc+1)
          east2 = tile ts (tr,tc+2)
          north1 = tile ts (tr-1,tc)
          north2 = tile ts (tr-2,tc)
          west1 = tile ts (tr,tc-1)
          west2 = tile ts (tr,tc-2)
          south1 = tile ts (tr+1,tc)
          south2 = tile ts (tr+2,tc)
          northwest22 = tile ts (tr-2,tc-2)
          northwest23 = tile ts (tr-2,tc-3)
          northeast22 = tile ts (tr-2,tc+2)
          northeast32 = tile ts (tr-3,tc+2)
          northeast42 = tile ts (tr-4,tc+2)
          southwest22 = tile ts (tr+2,tc-2)
          southeast22 = tile ts (tr-2,tc+2)
          southeast14 = tile ts (tr+1,tc+4)
          southwest41 = tile ts (tr+4,tc-1)
          southwest42 = tile ts (tr+4,tc-2)

edge :: [Tile] -> (Int,Int) -> Dir -> ((Int, Int), Int)
edge ts (tr,tc) d = check ts d (next (tr,tc) d)

npos2 :: [Tile] -> Position -> Position
npos2 ts (t, pos, dir)
    | deg == 0 && c < 1 = (etpos, (r,tsize), dir)
    | deg == 0 && c > tsize = (etpos, (r,1), dir)
    | deg == 0 && r < 1 = (etpos, (tsize,c), dir)
    | deg == 0 && r > tsize = (etpos, (1,c), dir)
    | deg == 90 && c < 1 = (etpos, (1,r), Down)
    | deg == 90 && c > tsize = (etpos, (tsize,r), Up)
    | deg == 90 && r < 1 = (etpos, (c,1), Right)
    | deg == 90 && r > tsize = (etpos, (tsize-c+1,1), Right)
    | deg == 180 && c < 1 = (etpos, (tsize-r+1,1), Right)
    | deg == 180 && c > tsize = (etpos, (tsize-r+1,tsize), Left)
    | deg == 180 && r < 1 = (etpos, (1,tsize-c+1), Down)
    | deg == 180 && r > tsize = (etpos, (tsize, tsize-c+1), Up)
    | deg == 270 && c < 1 = (etpos, (tsize,tsize-r+1), Up)
    | deg == 270 && c > tsize = (etpos, (1,tsize-r+1), Down)
    | deg == 270 && r < 1 = (etpos, (c,1), Right)
    | deg == 270 && r > tsize = (etpos, (c,tsize), Left)
    | otherwise = (t, (r,c), dir)
    where (r,c) = next pos dir
          tsize = ncols $ snd $ fromJust $ tile ts t
          (etpos,deg) = edge ts t dir

process (rs,path) = map (show.score board.solve) [npos1, npos2]
    where size = tilesize rs
          board = tiles size 0 rs
          start = (fst $ head board, (1,1), Right)
          steps = groupBy (\a b -> isAlpha a == isAlpha b) path
          solve f = foldl (follow board f) start steps

parse :: [String] -> ([String], String)
parse rows = (takeWhile ((>0).length) rows, last rows)

main :: IO ()
main = interact (unlines . process . parse . lines)
