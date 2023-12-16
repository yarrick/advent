import Data.Matrix
import Data.List
import Data.Char

getxy :: Matrix a -> a -> (Int,Int) -> a
getxy m fallback (x,y)
  | x <= 0 || y <= 0 || x > nrows m || y > ncols m = fallback
  | otherwise = getElem x y m

neighbors :: Matrix Int -> (Int,Int) -> [((Int,Int), Int)]
neighbors m (x,y) = [(pos, getxy m 99 pos) | pos <- diags (x,y) ]

diags :: (Int,Int) -> [(Int,Int)]
diags (x,y) = filter (/=(x,y)) [(a,b) | a <- [x-1,x,x+1], b <- [y-1,y,y+1] ]

flow :: (Int,Matrix Int) -> (Int,Matrix Int)
flow (c,m)
    | bs > c = flow (bs,ticked)
    | otherwise = (bs,ticked)
    where pos = [(r,c) | r <- [1..nrows m], c <- [1..ncols m] ]
          (bs,ticked) = foldl tick (c,m) pos

boom :: (Int,Int) -> (Int,Int) -> Int -> Int
boom center pos val
    | center == pos = 0
    | elem pos (diags center) && val > 0 = succ val
    | otherwise = val

tick :: (Int,Matrix Int) -> (Int,Int) -> (Int, Matrix Int)
tick (c,m) pos
    | cur < 10 = (c,m)
    | otherwise = (succ c, mapPos (boom pos) m)
    where cur = getxy m 0 pos

step :: (Int, Matrix Int) -> (Int, Matrix Int)
step (c,m) = flow (c, mapPos bump m)
    where bump _ val = succ val

allBlown :: (Int,(Int,Matrix Int)) -> Bool
allBlown (_,(_,m)) = sum (toList m) == 0

process :: [String] -> [String]
process rows = map show [fst $ ticked !! 100,
                         fst $ head $ filter allBlown $ zip [0..] ticked]
  where m = fromLists $ map (map digitToInt) rows
        ticked = iterate step $ (0, m)

main :: IO ()
main = interact (unlines . process . lines)
