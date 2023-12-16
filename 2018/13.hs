import Data.Char
import Data.List
import Data.Matrix

type Direction = (Int,Int)
up = (-1,0)
left = (0,-1)
right = (0,1)
down = (1,0)

corner :: Direction -> Char -> Direction
corner x '/'
    | x == down = left
    | x == right = up
    | x == left = down
    | x == up = right
corner x '\\'
    | x == down = right
    | x == right = down
    | x == left = up
    | x == up = left

startdir :: Char -> Direction
startdir '^' = up
startdir '<' = left
startdir '>' = right
startdir 'v' = down

move :: (Int,Int) -> Direction -> (Int,Int)
move (r,c) (dr,dc) = (r+dr, c+dc)

peek :: Matrix Char -> Car -> Char
peek m (pos,dir,_)
    | r < 1 || c < 1 || r > nrows m || c > ncols m = ' '
    | otherwise = m ! (r,c)
    where (r,c) = move pos dir

-- pos, direction, turn index (of left/up/right)
type Car = ((Int,Int), Direction, Int)

nextdir :: Car -> Direction
nextdir (_,dir,tx)
    | turn == up = dir
    | dir == up = turn
    | dir == down && turn == left = right
    | dir == down && turn == right = left
    | dir == left && turn == left = down
    | dir == left && turn == right = up
    | dir == right && turn == left = up
    | dir == right && turn == right = down
    where turns = [left, up, right]
          turn = turns !! (mod tx (length turns))

step :: Matrix Char -> Car -> Car
step m c@(pos,dir,tx)
    | elem nc "-|<>v^" = (move pos dir, dir, tx)
    | elem nc "/\\" = (move pos dir, corner dir nc,tx)
    | nc == '+' = (move pos dir, nextdir c, succ tx)
    where nc = peek m c

loc :: Car -> (Int,Int)
loc (p,_,_) = p

drive :: Matrix Char -> ([Car], [Car]) -> (Bool,[Car])
drive _ ([],c) = (False,c)
drive m (c:cs,done)
    | elem (loc cc) carlocs = (True, cc : cs ++ done)
    | otherwise = drive m (cs, cc:done)
    where cc = step m c
          carlocs = map loc $ cs ++ done

run :: Matrix Char -> [Car] -> ([Car], (Int,Int))
run m cars
    | crash = (moved, loc $ head moved)
    | otherwise = run m moved
    where cs = sort cars
          (crash, moved) = drive m (cs, [])
          locs = nub $ map loc moved

drive2 :: Matrix Char -> ([Car], [Car]) -> [Car]
drive2 _ ([],c) = c
drive2 m (c:cs,done)
    | elem (loc cc) carlocs = drive2 m (filter alive cs, filter alive done)
    | otherwise = drive2 m (cs, cc:done)
    where cc = step m c
          carlocs = map loc $ cs ++ done
          alive car = loc car /= loc cc

run2 :: Matrix Char -> [Car] -> Car
run2 m [a] = a
run2 m cars = run2 m $ drive2 m (cars,[])

process :: Matrix Char -> [String]
process m = map (show.crashsite) [snd $ run m cars, loc $ run2 m cars]
    where cells = [(r,c) | r <- [1..nrows m], c <- [1..ncols m] ]
          starts = filter (\(pp,c) -> elem c "<>^v") $ map (\p -> (p, m ! p)) cells
          cars = map (\(p,c) -> (p, startdir c, 0)) starts
          crashsite (r,c) = (c-1,r-1)

main :: IO ()
main = interact (unlines . process . fromLists . lines)
