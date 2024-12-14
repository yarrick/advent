import Data.Char
import Data.List
import qualified Data.Set as S

type Vec = (Int, Int) -- distance from left, from top

process a = map show [product $ map length quads, fst drawing]
    where room
            | length a < 20 = (11,7)
            | otherwise = (101, 103)
          seconds = zip [0..] $ iterate (map (step room)) a
          quads = group $ sort $ filter (>0) $ map (quad room) $ snd (seconds !! 100)
          drawing = head $ filter cluster seconds
          cluster (g,r)
            | length r < 20 = True
            | otherwise = (groupsize $ S.fromList $ map fst r) > 25

groupsize robos
    | S.null robos = 0
    | otherwise = maximum [length a, groupsize g]
    where pos = (head $ S.toList robos)
          (a, g) = grow [] [pos] (S.delete pos robos)

grow :: [Vec] -> [Vec] -> S.Set Vec -> ([Vec], S.Set Vec)
grow area [] robos = (area, robos)
grow prev ((r,c):todo) robos = grow ((r,c):prev) (todo++newsides) $ foldr S.delete robos ((r,c):newsides)
    where cands = [(r-1,c), (r,c-1), (r,c+1), (r+1,c)]
          known = filter (\p -> elem p (prev ++ todo)) cands
          newsides = filter (\p -> S.member p robos) cands

quad :: Vec -> (Vec, Vec) -> Int
quad (w,h) ((x,y),_)
    | x == wq || y == hq = 0
    | x < wq && y < hq = 1
    | x < wq && y > hq = 2
    | y < hq = 3
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
