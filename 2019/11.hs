import Data.List
import Data.Matrix

type State = (
  Integer,   -- pc
  [Integer], -- memory
  [Integer], -- inputs
  [Integer], -- output
  Integer)   -- relative base

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

getmem :: [Integer] -> Integer -> Integer
getmem mem p
  | otherwise = head newlist
  where newlist = drop (fromInteger p) mem

step :: State -> State
step d@(pc,mem,_,_,_)
  | op == 1 = compute (+) d immediate
  | op == 2 = compute (*) d immediate
  | op == 3 = input d immediate
  | op == 4 = output d immediate
  | op == 5 = jump (0/=) d immediate
  | op == 6 = jump (0==) d immediate
  | op == 7 = cmp (<) d immediate
  | op == 8 = cmp (==) d immediate
  | op == 9 = relbase d immediate
    where
      reg = getmem mem pc
      op = mod reg 100
      immediate = opflags reg

-- Always starts with a zero value to keep 1-based indexing
opflags :: Integer -> [Integer]
opflags reg = 0 : (take 3 $ map read $ map (\c -> [c]) $ drop 2 $ reverse $ ("0000" ++ show reg))

memset :: State -> Integer -> Integer -> Integer -> State
memset (pc,mem,inp,out,base) newpc pos value =
  (newpc, take (fromInteger pos) mem ++ [value] ++ drop (fromInteger (pos + 1)) mem, inp, out, base)

compute :: (Integer -> Integer -> Integer) -> State -> [Integer] -> State
compute op d@(pc,mem,inp,out,_) imm = memset d (pc+4) res (op a b)
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

-- immediate or position access
fetch :: State -> [Integer] -> Integer -> Integer
fetch (pc,mem,_,_,base) imm offset
  | getmem imm offset == 1 = reg
  | getmem imm offset == 2 = getmem mem (base + reg)
  | otherwise = getmem mem reg
    where reg = getmem mem (pc + offset)

outpos :: State -> [Integer] -> Integer -> Integer
outpos d@(pc,mem,_,_,base) imm offset
  | getmem imm offset == 2 = base + reg
  | otherwise = reg
    where reg = getmem mem (pc + offset)

input :: State -> [Integer] -> State
input d@(pc,mem,inp,out,base) imm = memset (pc,mem,tail inp,out,base) (pc+2) pos $ head inp
  where pos = outpos d imm 1

output :: State -> [Integer] -> State
output d@(pc,mem,inp,out,base) imm = (pc+2, mem, inp, out ++ [val], base)
  where val = fetch d imm 1

jump :: (Integer -> Bool) -> State -> [Integer] -> State
jump op d@(pc,mem,inp,out,base) imm
  | op arg = (pos, mem, inp, out, base)
  | otherwise = (pc+3, mem, inp, out, base)
    where
      arg = fetch d imm 1
      pos = fetch d imm 2

cmp :: (Integer -> Integer -> Bool) -> State -> [Integer] -> State
cmp op d@(pc,mem,inp,out,_) imm
  | op a b = memset d (pc+4) res 1
  | otherwise = memset d (pc+4) res 0
    where
      a = fetch d imm 1
      b = fetch d imm 2
      res = outpos d imm 3

relbase :: State -> [Integer] -> State
relbase d@(pc,mem,inp,out,base) imm = (pc+2,mem,inp,out, base + fetch d imm 1)

exec :: State -> State
exec d@(pc,mem,_,out,_)
  | length out > 1 = d -- stop when output twice
  | (getmem mem pc) == 99 = d
  | otherwise = exec $ step d

newstate :: [Integer] -> [Integer] -> State
newstate instr input = (0, instr ++ (take 1000 $ repeat 0), input, [], 0)

inputnum :: State -> Integer -> State
inputnum (pc,mem,inp,out,base) num = (pc,mem,inp ++ [num],out,base)

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
  | otherwise = paint (newrobot, (pc,mem,inp,[],base))
  where
    preppedstate = inputnum state (toInteger $ getcolor robot)
    newstate@(pc,mem,inp,out,base) = exec preppedstate
    getint pos = fromInteger (out !! pos)
    newrobot = turnrobot (paintsq robot (getint 0)) (getint 1)

run bytes = length $ filter (\(_,_,_,p) -> p) (s:sq)
  where
    (donerobot@(_,s,sq), donestate) = paint (newrobot Black, newstate (parse bytes) [])

-- part 2

iswhite :: [(Int,Int)] -> (Int,Int) -> Char
iswhite list (x,y)
  | elem (y,x) list = '#'
  | otherwise = ' '

letters pos = mx
  where mx = matrix 80 78 (iswhite pos)

run2 bytes = map (\r -> getRow r (letters whitepos)) [1..70]
  where
    (donerobot@(_,s,sq), donestate) = paint (newrobot White, newstate (parse bytes) [])
    whitepos = map (\(x,y,c,p) -> (x,20 - y)) $ filter (\(_,_,color,_) -> color == White) (s:sq)
