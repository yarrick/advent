import Intcode

type Square = (Integer, Integer, Integer)

newsquare :: [Integer] -> Square
newsquare (x:y:t:[]) = (x,y,t)

diffsquare :: Square -> Square -> Bool
diffsquare (a,b,_) (c,d,_) = a /= c || b /= d

samesquare :: Square -> Square -> Bool
samesquare (a,b,_) (c,d,_) = a == c && b == d

addsq :: [Square] -> Square -> [Square]
addsq old new = new : filter (diffsquare new) old

chunk :: [Integer] -> [[Integer]]
chunk [] = []
chunk list = (take 3 list) : (chunk $ drop 3 list)

tiletype :: Integer -> [Square] -> [Square]
tiletype n sq = filter (\(x,_,t) -> t == n && x >= 0) sq

numtiles :: Integer -> [Square] -> Int
numtiles n sq = length $ tiletype n sq

run bytes = (numtiles 2) $ foldl addsq [] squares
  where squares = map newsquare $ chunk $ outdata $ exec $ newstate (parse bytes) []

-- part 2

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

type Game = (State, [Square])

newgame :: [Integer] -> Game
newgame mem = (g, foldl addsq [] $ map newsquare $ chunk $ outdata g)
  where g = exec $ newhaltstate mem [] needinput

stepgame :: Game -> Integer -> Game
stepgame (st,sq) mov = (st2, foldl addsq sq $ map newsquare $ chunk $ outdata st2)
  where st2 = exec $ inputnum (st { outdata = [] }) mov

getscore :: Game -> Integer
getscore (_,sq) = (\(a,b,c) -> c) $ head $ filter (samesquare (-1,0,0)) sq

rungame :: Game -> Integer
rungame (st,sq)
  | numtiles 2 sq == 0 = getscore (st,sq)
  | ballcmp == EQ = rungame $ stepgame (st,sq) 0
  | ballcmp == LT = rungame $ stepgame (st,sq) (-1)
  | ballcmp == GT = rungame $ stepgame (st,sq) 1
  where xpos tt = (\(x,y,t) -> x) $ head $ tiletype tt sq
        ballcmp = compare (xpos 4) (xpos 3)

run2 bytes = rungame $ newgame $ toInteger 2 : (tail $ parse bytes)
