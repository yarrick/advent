import Data.Matrix
import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

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

run bytes = length $ filter (\(_,_,t) -> t == 2) $ foldl addsq [] squares
  where squares = map newsquare $ chunk $ outdata $ exec $ newstate (parse bytes) []

