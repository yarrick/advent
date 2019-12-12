import Data.List
import Data.Matrix
import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

data Direction = PointUp | PointDown | PointRight | PointLeft deriving (Eq,Show)
data Color = Black | White deriving (Eq,Show)

-- x, y, color, painted
type Square = (Int, Int, Color, Bool)

type Robot = (Direction, Square, [Square])

newsquare x y color = (x,y,color,False)

newrobot color = (PointUp, newsquare 0 0 color, [])

getcolor :: Robot -> Int
getcolor (_,(_,_,c,_),_)
  | c == Black = 0
  | c == White = 1

paintsq :: Robot -> Int -> Robot
paintsq (dir, (x,y,_,_), old) color
  | color == 0 = (dir, (x, y, Black, True), old)
  | color == 1 = (dir, (x, y, White, True), old)

turnrobot :: Robot -> Int -> Robot
turnrobot (PointUp,sq,old) turn
  | turn == 1 = moverobot PointRight (1,0) sq old
  | turn == 0 = moverobot PointLeft (-1,0) sq old
turnrobot (PointDown,sq,old) turn
  | turn == 1 = moverobot PointLeft (-1,0) sq old
  | turn == 0 = moverobot PointRight (1,0) sq old
turnrobot (PointLeft,sq,old) turn
  | turn == 1 = moverobot PointUp (0,1) sq old
  | turn == 0 = moverobot PointDown (0,-1) sq old
turnrobot (PointRight,sq,old) turn
  | turn == 1 = moverobot PointDown (0,-1) sq old
  | turn == 0 = moverobot PointUp (0,1) sq old

samesquare :: Square -> Square -> Bool
samesquare (a,b,_,_) (c,d,_,_) = a == c && b == d

moverobot :: Direction -> (Int,Int) -> Square -> [Square] -> Robot
moverobot dir (xdiff,ydiff) cur@(x,y,_,_) old
  | length oldpos > 0 = (dir, head oldpos, cur : filter (\s -> not (samesquare newpos s)) old)
  | otherwise = (dir, newpos, cur : old)
  where
    newpos = newsquare (x+xdiff) (y+ydiff) Black
    oldpos = filter (samesquare newpos) old

paint :: (Robot, State) -> (Robot,State)
paint (robot, state)
  | length out < 2 = (robot, newstate)
  | otherwise = paint (newrobot, (pc,mem,inp,[],base,h))
  where
    preppedstate = inputnum state (toInteger $ getcolor robot)
    newstate@(pc,mem,inp,out,base,h) = exec preppedstate
    getint pos = fromInteger (out !! pos)
    newrobot = turnrobot (paintsq robot (getint 0)) (getint 1)

haltcpu :: [Integer] -> Bool
haltcpu out = length out > 1

run bytes = length $ filter (\(_,_,_,p) -> p) (s:sq)
  where
    (donerobot@(_,s,sq), donestate) = paint (newrobot Black, newhaltstate (parse bytes) [] haltcpu)

-- part 2

iswhite :: [(Int,Int)] -> (Int,Int) -> Char
iswhite list (x,y)
  | elem (y,x) list = '#'
  | otherwise = ' '

letters pos = mx
  where mx = matrix 80 78 (iswhite pos)

run2 bytes = map (\r -> getRow r (letters whitepos)) [1..70]
  where
    (donerobot@(_,s,sq), donestate) = paint (newrobot White, newhaltstate (parse bytes) [] haltcpu)
    whitepos = map (\(x,y,c,p) -> (x,20 - y)) $ filter (\(_,_,color,_) -> color == White) (s:sq)
