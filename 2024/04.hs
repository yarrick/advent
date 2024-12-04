import Data.Matrix

process :: Matrix Char -> [String]
process m = map (show.sum) [map locate $ chars 'X', map (masx m) $ chars 'A' ]
    where chars cc = filter (\p -> m ! p == cc) [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]
          locate x = sum $ map (\d -> xmas m "MAS" d x)
                 [(pred,pred), (pred,id), (pred,succ),
                  (id,pred),              (id,succ),
                  (succ,pred), (succ,id), (succ,succ)]

xmas :: Matrix Char -> [Char] -> (Int ->Int, Int -> Int) -> (Int, Int) -> Int
xmas _ [] _ _ = 1
xmas m (t:ts) (stepr,stepc) (r,c)
    | nr < 1 || nr > nrows m || nc < 1 || nc > ncols m = 0
    | m ! (nr,nc) /= t = 0
    | otherwise = xmas m ts (stepr,stepc) (nr,nc)
    where nr = stepr r
          nc = stepc c

masx :: Matrix Char -> (Int, Int) -> Int
masx m (r,c)
    | r <= 1 || r >= nrows m || c <= 1 || c >= ncols m = 0
    | elem 'M' a && elem 'M' b && elem 'S' a && elem 'S' b = 1
    | otherwise = 0
    where a = m ! (pred r, pred c) : m ! (succ r, succ c) : []
          b = m ! (pred r, succ c) : m ! (succ r, pred c) : []

main :: IO ()
main = interact (unlines . process . fromLists . lines)
