import Data.Matrix
import Data.Bits

generate :: Int -> Int -> (a,a) -> Matrix a
generate input size def = matrix size size (wall input def)

wall :: Int -> (a,a) -> (Int,Int) -> a
wall input def (i,j) = wallxy input def (j-1,i-1)

wallxy :: Int -> (a,a) -> (Int,Int) -> a
wallxy input (open,blocked) (x,y)
  | even $ popCount num = open
  | otherwise = blocked
  where
    num = x*x + 3*x + 2*x*y + y + y*y + input

mark :: Matrix a -> (Int,Int) -> a -> Matrix a
mark m (x,y) val = setElem val (y+1,x+1) m

mmark :: Matrix a -> a -> [(Int,Int)] -> Matrix a
mmark m _ [] = m
mmark m val (x:xs) = mmark (mark m x val) val xs

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 0 = fallback
  | y < 0 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem (y+1) (x+1) m

peers :: Matrix Int -> (Int,Int) -> Int -> [(Int,Int)]
peers m (x,y) val = map fst $ filter (\x -> snd x > val) neighbors
  where
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = zip cross (map (\pos -> getxy m pos (-1)) cross)

spread :: Matrix Int -> (Int,Int) -> Matrix Int
spread m pos
  | self == 999999 = m
  | self == -1 = m
  | otherwise = mmark m (self+1) $ peers m pos self
  where self = getxy m pos (-1)

spreader :: Matrix Int -> [(Int,Int)] -> Matrix Int
spreader m [] = m
spreader m (p:ps) = spreader (spread m p) ps

allpos :: Matrix Int -> [(Int,Int)]
allpos m = concat $ map (\x -> zip (repeat x) pos) pos
  where pos = [0..(nrows m)]

step :: Matrix Int -> Matrix Int
step m = spreader m $ allpos m

flow :: Matrix Int -> Matrix Int
flow m
  | m == newm = m
  | otherwise = flow newm
  where newm = step m

run :: Int -> Int
run input = getxy solved (1,1) (-1)
  where
    m = generate input 50 (999999,-1)
    solved = flow (mark m (31,39) 0)

-- part 2

reachable :: Matrix Int -> Int
reachable m = length $ filter (0<=) $ filter (50>=) values
  where values = map (\pos -> getxy m pos (-1)) $ allpos m

run2 :: Int -> Int
run2 input = reachable solved
  where
    m = generate input 50 (999999,-1)
    solved = flow (mark m (1,1) 0)

process :: [String] -> [String]
process (row:_) = map show [run num, run2 num]
    where num = read row

main :: IO ()
main = interact (unlines . process . lines)
