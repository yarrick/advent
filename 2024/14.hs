import Data.Char
import Data.List

type Vec = (Int, Int) -- distance from left, from top

process a = [show $ product $ map length quads]
    where room
            | length a < 20 = (11,7)
            | otherwise = (101, 103)
          seconds = zip [0..] $ iterate (map (step room)) a
          quads = group $ sort $ filter (>0) $ map (quad room) $ snd (seconds !! 100)

quad :: Vec -> (Vec, Vec) -> Int
quad (w,h) ((x,y),_)
    | x == wq || y == hq = 0
    | x < wq && y < hq = 1
    | x < wq && y > hq = 2
    | x > wq && y < hq = 3
    | otherwise = 4
    where (wq,hq) = (div w 2, div h 2)

step :: Vec -> (Vec, Vec) -> (Vec, Vec)
step (w,h) ((x,y), (dx,dy)) = ((mod (x+dx) w, mod (y+dy) h), (dx,dy))

parse :: String -> (Vec, Vec)
parse s = ((read (ns!!1), read (ns!!3)),(read (ns!!5), read (ns!!7)))
    where isNumPart c = isDigit c || c == '-'
          ns = groupBy (\a b -> isNumPart a == isNumPart b) s

main :: IO ()
main = interact (unlines . process . map parse . lines)
