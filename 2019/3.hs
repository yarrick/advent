import Data.List

parse :: String -> [(Direction,Int)]
parse [] = []
parse w = (dir $ head move, read $ tail move) : parse (drop 1 end)
    where (move, end) = break (','==) w

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show)

dir :: Char -> Direction
dir 'U' = GoUp
dir 'D' = GoDown
dir 'R' = GoRight
dir 'L' = GoLeft

expand :: Int -> Int -> [(Direction,Int)] -> [(Int,Int)]
expand _ _ [] = []
expand x y ((d,len):rest) = zip xlist ylist ++ expand (last xlist) (last ylist) rest
  where
    xlist = xpos x d len
    ylist = ypos y d len

xpos :: Int -> Direction -> Int -> [Int]
xpos start dir len
  | dir == GoUp || dir == GoDown = replicate len start
  | dir == GoLeft = map (\a -> start - a) steps
  | dir == GoRight = map (\a -> start + a) steps
    where steps = take len [1..]

ypos :: Int -> Direction -> Int -> [Int]
ypos start dir len
  | dir == GoLeft || dir == GoRight = replicate len start
  | dir == GoDown = map (\a -> start - a) steps
  | dir == GoUp = map (\a -> start + a) steps
    where steps = take len [1..]

wire :: String -> [(Int,Int)]
wire moves = sort $ expand 0 0 $ parse moves

run :: String -> String -> Int
run a b = minimum $ map (\(x,y) -> (abs x) + (abs y)) $ cross (wire a) (wire b)

-- like List intersect but for sorted lists
cross :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
cross [] _ = []
cross _ [] = []
cross x@((a,b):xs) y@((c,d):ys)
  | a == c && b == d = (a,b) : cross xs ys
  | a > c = cross x ys
  | c > a = cross xs y
  | a == c && b > d = cross x ys
  | a == c && b < d = cross xs y


-- part 2

wirelen :: String -> [((Int,Int),Int)]
wirelen moves = sort $ zip (expand 0 0 $ parse moves) [1..]

crosslen :: [((Int,Int),Int)] -> [((Int,Int),Int)] -> [(Int,Int,Int)]
crosslen [] _ = []
crosslen _ [] = []
crosslen x@(((a,b),p):xs) y@(((c,d),q):ys)
  | a == c && b == d = (a,b,p+q) : crosslen xs ys
  | a > c = crosslen x ys
  | c > a = crosslen xs y
  | a == c && b > d = crosslen x ys
  | a == c && b < d = crosslen xs y

run2 :: String -> String -> Int
run2 a b = minimum $ map (\(x,y,len) -> len) $ crosslen (wirelen a) (wirelen b)
