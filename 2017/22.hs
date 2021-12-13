import qualified Data.Map as M

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
type Grid = M.Map (Int,Int) Char

getElem :: (Int,Int) -> Grid -> Char
getElem pos m = M.findWithDefault '.' pos m

setElem :: Char -> (Int,Int) -> Grid -> Grid
setElem c pos m = M.insert pos c m

step :: (Grid, Carrier) -> (Grid, Carrier)
step (m, (pos,dir,infects))
    | infected = (setElem '.' pos m, mover goright id)
    | otherwise = (setElem '#' pos m, mover goleft succ)
    where infected = '#' == getElem pos m
          mover turn stats = (move pos (turn dir), turn dir, stats infects)

step2 :: (Grid, Carrier) -> (Grid, Carrier)
step2 (m, (pos,dir,infects))
    | state == '.' = (setElem 'W' pos m, mover goleft id)
    | state == 'W' = (setElem '#' pos m, mover id succ)
    | state == '#' = (setElem 'F' pos m, mover goright id)
    | state == 'F' = (setElem '.' pos m, mover (goright.goright) id)
    where state = getElem pos m
          mover turn stats = (move pos (turn dir), turn dir, stats infects)

parse rows = M.fromList $ concat $ map addrow $ zip [0..] cols
    where cols = map (zip [0..]) rows
          addrow (r,cvs) = map (\c -> ((r,fst c),snd c)) cvs

process :: Grid -> [String]
process m = map show [infected, inf2]
    where (maxrow,maxcol) = fst $ last $ M.toList m
          car = ((div maxrow 2, div maxcol 2), up, 0)
          (vm,(pos,dir,infected)) = (iterate step (m,car)) !! 10000
          (vm2,(p2,d2,inf2)) = (iterate step2 (m,car)) !! 10000000

main :: IO ()
main = interact (unlines . process . parse . lines)
