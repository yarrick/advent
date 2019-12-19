import Prelude hiding (Left,Right)
import Intcode
import Data.Char
import Data.Matrix

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

square n = [ (x,y) | x <- [0..(n-1)], y <- [0..(n-1)] ]

checkpos mem (x,y) = outdata $ exec state
  where state = newstate mem [x,y]

run bytes = sum $ concat $ map (checkpos mem) (square 50)
  where mem = parse bytes

