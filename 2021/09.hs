import Data.Matrix
import Data.List
import Data.Char

getxy :: Matrix a -> a -> (Int,Int) -> a
getxy m fallback (x,y)
  | x < 0 || y < 0 || x >= nrows m || y >= ncols m = fallback
  | otherwise = getElem (x+1) (y+1) m

neighbors :: Matrix Int -> (Int,Int) -> [((Int,Int), Int)]
neighbors m (x,y) = [(pos, getxy m 99 pos) | pos <- [(x-1,y), (x,y-1), (x+1,y), (x,y+1)] ]

basin :: Matrix Int -> (Int,Int) -> [(Int,Int)]
basin m pos = pos : concat (map (basin m.fst) $ higher pos)
  where higher pos = filter (\(_,v) -> v > getxy m 0 pos && v < 9) $ neighbors m pos

process :: [String] -> [String]
process rows = map show [sum $ map (succ.getxy m 0) $ lows,
                         product $ take 3 $ reverse $ sort $ map (length.nub.basin m) lows]
  where m = fromLists $ map (map digitToInt) rows
        lowpoint pos = all (>getxy m 0 pos) $ map snd $ neighbors m pos
        lows = filter lowpoint [(r,c) | r <- [0..nrows m-1], c <- [0..ncols m-1] ]

main :: IO ()
main = interact (unlines . process . lines)
