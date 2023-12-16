import Data.Char
import Data.List

path :: ((Int,Int),(Int,Int),Int) -> ([Int],[Int]) -> [Int]
path ((x,y),(dx,dy),maxy) (xr,yr)
    | newy < (head yr) = []
    | elem newx xr && elem newy yr = [maxy]
    | otherwise = path ((newx,newy),(drag,pred dy),maximum [maxy,newy]) (xr,yr)
    where newx = x+dx
          newy = y+dy
          drag
            | dx > 0 = pred dx
            | dx < 0 = succ dx
            | otherwise = 0

process :: ([Int],[Int]) -> [String]
process (xr,yr) = map show [maximum hits, length hits]
    where try dir = path ((0,0),dir,0) (xr,yr)
          hits = concat [try (x,y) | x <- [0..(last xr)], y <- [(head yr)..1000] ]

parse :: String -> ([Int], [Int])
parse str = ([(nums!!0)..(nums!!1)],[(nums!!2)..(nums!!3)])
    where ns = groupBy (\a b -> isnum a == isnum b) str
          isnum c = isDigit c || c == '-'
          nums = map read $ filter (\n -> isnum (head n)) ns

main :: IO ()
main = interact (unlines . process . parse . head . lines)
