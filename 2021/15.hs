import Data.Matrix
import Data.Char
import Data.List
import Control.DeepSeq

getxy :: Matrix a -> a -> (Int,Int) -> a
getxy m fallback (x,y)
  | x <= 0 || y <= 0 || x > nrows m || y > ncols m = fallback
  | otherwise = getElem x y m

neighbors :: Matrix Int -> (Int,Int) -> [((Int,Int), Int)]
neighbors m (x,y) = [((c,r), getxy m 99999 (c,r)) | (c,r) <- [(x-1,y), (x,y-1), (x+1,y), (x,y+1)],
                     c >= 1 && c <= ncols m && r >= 1 && r <= nrows m ]

update :: Matrix Int -> (Matrix Int, [(Int,Int)]) -> (Int,Int) -> (Matrix Int, [(Int,Int)])
update rs (ss,cs) pos
    | newsum < 99999 && newsum < cursum = (setElem newsum pos ss, cs ++ npos)
    | otherwise = (ss, cs)
    where rval = getxy rs 9999 pos
          (npos,nvals) = unzip $ neighbors ss pos
          cursum = getxy ss 99999 pos
          newsum = rval + minimum nvals

flow :: Matrix Int -> Matrix Int -> [(Int,Int)] -> Matrix Int
flow rs m cells
    | next == m = m
    | otherwise = flow rs (deepseq next next) (nub nc)
    where (next,nc) = foldl (update rs) (m,[]) cells

solve :: Matrix Int -> Int
solve starter = getxy summed 0 (1,1)
    where risk = setElem 0 (1,1) starter
          rs = nrows risk
          cs = ncols risk
          sums = setElem (getxy risk 0 (rs,cs)) (rs,cs) $ matrix rs cs (\_ -> 99999)
          summed = flow risk sums (map fst $ neighbors sums (ncols sums, nrows sums))

-- messier due to 1-based indexing
grow :: Matrix Int -> Matrix Int
grow g = matrix (5*rs) (5*cs) copier
    where rs = nrows g
          cs = ncols g
          copier pos@(r,c)
            | r <= rs && c <= cs = getxy g (-5) pos
            | otherwise = clamp $ getxy g (-9) (wrap r rs,wrap c cs) + steps
            where steps = (div (r-1) rs) + (div (c-1) cs)
                  wrap p len = 1 + (p-1) `mod` len
                  clamp n
                    | n <= 9 = n
                    | otherwise = mod n 9

process :: [String] -> [String]
process rows = map show [solve m, solve $ grow m]
  where m = fromLists $ map (map digitToInt) rows

main :: IO ()
main = interact (unlines . process . lines)
