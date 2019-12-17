import Intcode
import Data.Char
import Data.Matrix

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

getgraph :: [String] -> (Int,Int) -> Char
getgraph gr (y,x) = (gr !! (y-1)) !! (x-1)

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem x y m

peers :: Matrix Char -> (Int,Int) -> [(Int,Int)]
peers m (x,y)
  | getxy m (x,y) '.' == '#' && length neighbors == 4 = [(x,y)]
  | otherwise = []
  where
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = filter ('#'==) $ map (\pos -> getxy m pos '.') cross

score :: Matrix a -> [(Int,Int)] -> [Int]
score _ [] = []
score m ((x,y):ps) = (x-1)*(y-1) : score m ps

run bytes = sum $ score m $ concatMap (peers m) $ [ (x,y) | x <- [1..(nrows m)], y <- [1..(ncols m)]]
  where state = exec $ newstate (parse bytes) []
        drawing = lines $ map (chr . fromInteger) $ outdata state
        graph = filter (\x -> length x == length (head drawing)) drawing
        m = matrix (length graph) (length (head graph)) (getgraph graph)
