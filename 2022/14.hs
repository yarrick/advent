import Data.Char
import Data.List
import Data.Matrix

stone :: [(Int,Int)] -> [(Int,Int)]
stone (b:[]) = []
stone ((sr,sc):(er,ec):cs)
    | sc == ec && sr < er = [(r,sc) | r <- [sr..er] ] ++ stone ((er,ec):cs)
    | sc == ec = [(r,sc) | r <- [er..sr] ] ++ stone ((er,ec):cs)
    | sc < ec = [(sr,c) | c <- [sc..ec] ] ++ stone ((er,ec):cs)
    | otherwise = [(sr,c) | c <- [ec..sc] ] ++ stone ((er,ec):cs)

draw :: Int -> Int -> [[(Int,Int)]] -> Matrix Char
draw nr nc coords = matrix nr nc filler
    where stonepos = nub $ concat $ map stone coords
          filler p
            | elem p stonepos = '#'
            | otherwise = ' '

drip :: Matrix Char -> (Int,Int) -> (Matrix Char, Bool)
drip m (r,c)
    | r >= nrows m = (m, True)
    | below == ' ' = drip m (succ r, c)
    | diagleft == ' ' = drip m (succ r, pred c)
    | diagright == ' ' = drip m (succ r, succ c)
    | otherwise = (setElem 'o' (r,c) m, r == 1)
    where below = getElem (succ r) c m
          diagleft = getElem (succ r) (pred c) m
          diagright = getElem (succ r) (succ c) m

spill :: Matrix Char -> Int -> (Int,Int) -> (Int, Matrix Char)
spill m sand start
    | overflow = (succ sand, nm)
    | otherwise = spill nm (succ sand) start
    where (nm, overflow) = drip m start

solve :: [[(Int,Int)]] -> Int
solve rows = fst $ spill m 0 $ transl (500,0)
    where depth = succ $ succ $ maximum $ map snd $ concat rows
          cols = sort $ map fst $ concat rows
          width = maximum cols - minimum cols + 4
          transl (c,r) = (r + 1, c - minimum cols + 2)
          m = draw depth width $ map (map transl) rows

process :: [[(Int,Int)]] -> [String]
process rows = map show [solve rows, solve (floor:rows)]
    where depth = succ $ succ $ maximum $ map snd $ concat rows
          floor = [(500-(2*depth),depth),(500+(2*depth),depth)]

parse :: String -> [(Int,Int)]
parse s = map (\c -> read ("("++c++")")) $ filter (isDigit.head) $ words s

main :: IO ()
main = interact (unlines . process . map parse . lines)
