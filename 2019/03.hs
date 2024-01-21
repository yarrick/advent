import Prelude hiding (Left,Right)
import Data.List

data Direction = Up | Down | Left | Right deriving (Eq, Show)

parse :: String -> [(Direction,Int)]
parse [] = []
parse w = (dir $ head move, read $ tail move) : parse (drop 1 end)
    where (move, end) = break (','==) w
          dir c = snd $ head $ filter (\f -> fst f == c) [('U',Up),('D',Down),('R',Right),('L',Left)]

expand :: Int -> Int -> [(Direction,Int)] -> [(Int,Int)]
expand _ _ [] = []
expand x y ((d,len):rest) = zip xlist ylist ++ expand (last xlist) (last ylist) rest
  where
    xlist = dpos ([Up,Down],Left,Right) x d len
    ylist = dpos ([Left,Right],Down,Up) y d len

dpos :: ([Direction],Direction,Direction) -> Int -> Direction -> Int -> [Int]
dpos (zero,neg,pos) start dir len
  | elem dir zero = replicate len start
  | dir == neg = map (\a -> start - a) steps
  | dir == pos = map (\a -> start + a) steps
    where steps = take len [1..]

wirelen :: String -> [((Int,Int),Int)]
wirelen moves = sort $ zip (expand 0 0 $ parse moves) [1..]

crosslen :: [((Int,Int),Int)] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
crosslen [] _ = []
crosslen _ [] = []
crosslen x@(((a,b),p):xs) y@(((c,d),q):ys)
  | a == c && b == d = ((a,b),p+q) : crosslen xs ys
  | a > c = crosslen x ys
  | c > a = crosslen xs y
  | a == c && b > d = crosslen x ys
  | a == c && b < d = crosslen xs y

process :: [String] -> [String]
process (a:b:_) = map (show.minimum) [map dist crosses, map snd crosses]
    where crosses = crosslen (wirelen a) (wirelen b)
          dist ((x,y),_) = abs x + abs y

main :: IO ()
main = interact (unlines . process . lines)
