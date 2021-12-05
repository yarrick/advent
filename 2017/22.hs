import Data.Matrix

type Direction = (Int,Int)
up = (-1,0)
left = (0,-1)
right = (0,1)
down = (1,0)

goright :: Direction -> Direction
goright d
    | d == down = left
    | d == right = down
    | d == left = up
    | d == up = right

goleft :: Direction -> Direction
goleft d
    | d == down = right
    | d == right = up
    | d == left = down
    | d == up = left

move :: (Int,Int) -> Direction -> (Int,Int)
move (r,c) (dr,dc) = (r+dr, c+dc)

-- pos (r,c), direction, infections
type Carrier = ((Int,Int), Direction, Int)

step :: (Matrix Char, Carrier) -> (Matrix Char, Carrier)
step (m, (pos@(r,c),dir,infects))
    | infected = (setElem '.' pos m, mover goright id)
    | otherwise = (setElem '#' pos m, mover goleft succ)
    where infected = '#' == getElem r c m
          mover turn stats = (move pos (turn dir), turn dir, stats infects)

insertInput :: Matrix Char -> (Int,Int) -> Char
insertInput m (r,c)
    | r <= 100 || c <= 100 = '.'
    | r > 100 + nrows m || c > 100 + ncols m = '.'
    | otherwise = getElem (r-100) (c-100) m

process :: Matrix Char -> [String]
process m = [show infected]
    where start = matrix 500 500 (insertInput m)
          car = ((101 + div (nrows m) 2, 101 + div (ncols m) 2), up, 0)
          (vm,(pos,dir,infected)) = (iterate step (start,car)) !! 10000

main :: IO ()
main = interact (unlines . process . fromLists . lines)
