import Data.Char
import Data.Matrix

type Direction = (Int,Int)
up = (-1,0)
left = (0,-1)
right = (0,1)
down = (1,0)

turns :: Direction -> [Direction]
turns dir
    | elem dir [up,down] = [left,right]
    | otherwise = [up,down]

step :: (Int,Int) -> Direction -> (Int,Int)
step (r,c) (dr,dc) = (r+dr, c+dc)

peek :: Matrix Char -> (Int,Int) -> Direction -> Char
peek m pos dir
    | r < 1 || c < 1 || r > nrows m || c > ncols m = ' '
    | otherwise = m ! (r,c)
    where (r,c) = step pos dir

walk :: Matrix Char -> (Int,Int) -> Direction -> String
walk m pos dir
    | not (elem nc " +") = nc : walk m (step pos dir) dir
    | length options == 0 = []
    | otherwise = nc : walk m (step pos newdir) newdir
    where nc = m ! pos
          options = filter (\(_,c) -> c /= ' ') [ (d, peek m pos d) | d <- turns dir ]
          (newdir, nnc) = head options

process :: Matrix Char -> [String]
process m = [filter isAlpha path, show $ length path]
    where startCols = zip [1..] $ map (\c -> m ! (1,c)) [1..ncols m]
          startPos = (1, fst $ head $ filter (\(_,c) -> c == '|') startCols)
          path = (m!startPos) : (walk m (step startPos down) down)

main :: IO ()
main = interact (unlines . process . fromLists . lines)
