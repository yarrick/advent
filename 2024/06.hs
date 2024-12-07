import Prelude hiding (Left, Right)
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col
data Dir = Up | Right | Down | Left deriving (Enum,Eq,Show)

process :: (Int, Int, Pos, M.Map Pos Char) -> [String]
process (nrows, ncols, start, walls) = map show [M.size visited, length $ filter loops places]
    where visited = walk (nrows, ncols, walls) M.empty start Up
          places = M.keys $ M.delete start visited
          loops p = loopable (nrows, ncols, M.insert p '#' walls) 0 start Up

walk :: (Int, Int, M.Map Pos Char) -> (M.Map Pos Dir) -> Pos -> Dir -> (M.Map Pos Dir)
walk (nrows, ncols, walls) visited pos dir
    | fst npos < 0 || fst npos >= nrows || snd npos < 0 || snd npos >= ncols = visited
    | M.member npos walls = walk (nrows,ncols,walls) visited pos (turn dir)
    | otherwise = walk (nrows,ncols,walls) (M.insert npos dir visited) npos dir
    where npos = step pos dir

loopable (nrows, ncols, walls) len pos dir
    | fst npos < 0 || fst npos >= nrows || snd npos < 0 || snd npos >= ncols = False
    | len > 10000 = True
    | M.member npos walls = loopable (nrows,ncols,walls) len pos (turn dir)
    | otherwise = loopable (nrows,ncols,walls) (succ len) npos dir
    where npos = step pos dir

step (r,c) Up = (r-1,c)
step (r,c) Down = (r+1,c)
step (r,c) Right = (r,c+1)
step (r,c) Left = (r,c-1)

turn Left = Up
turn d = succ d

parse :: [String] -> (Int, Int, Pos, M.Map Pos Char)
parse ss = (length ss, length (head ss), fst $ head $ select '^', M.fromList $ select '#')
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) ss
          select ch = filter (\(a,b) -> b == ch) cells

main :: IO ()
main = interact (unlines . process . parse . lines)
