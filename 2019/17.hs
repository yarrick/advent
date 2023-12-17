import Prelude hiding (Left,Right)
import Intcode
import Data.Matrix

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x > nrows m = fallback
  | y > ncols m = fallback
  | otherwise = getElem x y m

cross :: Matrix Char -> (Int,Int) -> [(Int,Int)]
cross m (x,y)
  | getxy m (x,y) '.' == '.' = []
  | length neighbors == 4 = [(x,y)]
  | otherwise = []
  where
    peers = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = filter ('#'==) $ map (\pos -> getxy m pos '.') peers

score :: Matrix a -> [(Int,Int)] -> [Int]
score _ [] = []
score m ((x,y):ps) = (x-1)*(y-1) : score m ps

run bytes = sum $ score m $ concatMap (cross m) $ [ (x,y) | x <- [1..(nrows m)], y <- [1..(ncols m)]]
  where state = exec $ newstate (parse bytes) []
        drawing = outputstr state
        graph = filter (\x -> length x == length (head drawing)) drawing
        m = fromList (length graph) (length (head graph)) (concat graph)

-- part 2

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

findbot :: Matrix Char -> ((Int,Int),Direction)
findbot m
  | snd bot == '^' = (fst bot,Up)
  | snd bot == 'v' = (fst bot,Down)
  | snd bot == '>' = (fst bot,Right)
  | snd bot == '<' = (fst bot,Left)
  where bot = head $ filter (\(_,c) -> elem c "<>^v") [ ((x,y), getElem x y m) | x <- [1..(nrows m)], y <- [1..(ncols m)]]

next :: ((Int,Int),Direction) -> (Int,Int)
next ((x,y),Up) =  (x-1, y)
next ((x,y),Down) = (x+1, y)
next ((x,y),Left) = (x, y-1)
next ((x,y),Right) = (x, y+1)

turn :: ((Int,Int),Direction) -> Matrix Char -> (((Int,Int),Direction),Char)
turn (pos,dir) m
  | dir == Down && getxy m (next (pos,Left)) '/' == '#' = ((pos,Left),'R')
  | dir == Down && getxy m (next (pos,Right)) '/' == '#' = ((pos,Right),'L')
  | dir == Up && getxy m (next (pos,Left)) '/' == '#' = ((pos,Left),'L')
  | dir == Up && getxy m (next (pos,Right)) '/' == '#' = ((pos,Right),'R')
  | dir == Left && getxy m (next (pos,Up)) '/' == '#' = ((pos,Up),'R')
  | dir == Left && getxy m (next (pos,Down)) '/' == '#' = ((pos,Down),'L')
  | dir == Right && getxy m (next (pos,Up)) '/' == '#' = ((pos,Up),'L')
  | dir == Right && getxy m (next (pos,Down)) '/' == '#' = ((pos,Down),'R')
  | otherwise = ((pos,dir),'@') -- goal

walk :: ((Int,Int),Direction) -> Matrix Char -> String
walk (pos,dir) m
  | getxy m (next (pos,dir)) '.' == '.' && turn (pos,dir) m == ((pos,dir),'@') = []
  | getxy m (next (pos,dir)) '.' == '.' = tl : walk (tp,td) m
  | otherwise = show (length cells) ++ walk (last cells) m
  where ((tp,td),tl) = turn (pos,dir) m
        forward (fp,fd) = (next (fp,fd),fd)
        cells = takeWhile (\(p,d) -> getxy m p '/' == '#') $ tail $ iterate forward (pos,dir)

-- prints path, compress manually, and update a/b/c/main below
path bytes = walk (findbot m) m
  where state = exec $ newhaltstate (2 : tail (parse bytes)) [] needinput
        drawing = outputstr state
        graph = filter (\x -> length x == length (head drawing)) drawing
        m = fromList (length graph) (length (head graph)) (concat graph)

run2 bytes = last $ outdata $ exec $ foldl inputstr state [main,a,b,c,"n"]
  where state = exec $ newhaltstate (2 : tail (parse bytes)) [] needinput
        a = "L,6,R,8,R,12,L,6,L,8"
        b = "L,10,L,8,R,12"
        c = "L,8,L,10,L,6,L,6"
        main = "A,B,A,C,B,C,B,A,C,B"

process :: String -> [String]
process rows = [show $ run rows, show $ run2 rows]

main :: IO ()
main = interact (unlines . process)
