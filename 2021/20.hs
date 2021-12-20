import Data.Matrix

getxy :: Matrix a -> a -> (Int,Int) -> a
getxy m fallback (r,c)
  | r < 1 || c < 1 || r > nrows m || c > ncols m = fallback
  | otherwise = getElem r c m

neighbors :: (Matrix Int,Int) -> (Int,Int) -> [Int]
neighbors (m,def) (r,c) = [getxy m def (rr,cc) | rr <- [r-1..r+1], cc <- [c-1..c+1] ]

enhance :: String -> (Matrix Int,Int) -> (Matrix Int,Int)
enhance alg (m,def) = (matrix (nrows m) (ncols m) compute, compute (1,1))
    where compute pos
            | alg !! pval == '#' = 1
            | otherwise = 0
            where pval = foldl (\a b -> 2*a+b) 0 $ neighbors (m,def) pos

process :: (String, (Matrix Int,Int)) -> [String]
process (alg,(m,def)) = map show [lit 2, lit 50]
    where steps = iterate (enhance alg) (m,def)
          lit n = sum $ toList $ fst $ steps !! n

parse :: [String] -> (String, (Matrix Int,Int))
parse (alg:_:state) = (alg, (matrix (extra + nrows mchar) (extra + ncols mchar) copier,0))
    where mchar = fromLists state
          (extra,base) = (110, 55)
          copier (r,c)
            | rr < 1 || rr > nrows mchar || cc < 1 || cc > ncols mchar = 0
            | getElem rr cc mchar == '#' = 1
            | otherwise = 0
            where (rr,cc) = (r - base, c - base)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

