import qualified Knot as K
import Data.Matrix
import Data.List

bits :: Char -> [Int]
bits '0' = [0,0,0,0]
bits '1' = [0,0,0,1]
bits '2' = [0,0,1,0]
bits '3' = [0,0,1,1]
bits '4' = [0,1,0,0]
bits '5' = [0,1,0,1]
bits '6' = [0,1,1,0]
bits '7' = [0,1,1,1]
bits '8' = [1,0,0,0]
bits '9' = [1,0,0,1]
bits 'a' = [1,0,1,0]
bits 'b' = [1,0,1,1]
bits 'c' = [1,1,0,0]
bits 'd' = [1,1,0,1]
bits 'e' = [1,1,1,0]
bits 'f' = [1,1,1,1]

binhash :: String -> [Int]
binhash str = concatMap bits $ K.knot str

neighbors :: Matrix Int -> (Int,Int) -> [(Int,Int)]
neighbors m (xs,ys) = filter belongs $ filter validY $ filter validX possible
    where possible = [(xs,y) | y <- [ys-1,ys+1] ] ++ [(x,ys) | x <- [xs-1,xs+1] ]
          validX (r,_) = r >= 1 && r < nrows m
          validY (_,c) = c >= 1 && c < ncols m
          belongs p = m ! p > 0

cells :: Matrix Int -> [(Int,Int)]
cells m = filter open [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]
    where open p = m ! p >= 0

flow :: Matrix Int -> (Int,Int) -> Matrix Int
flow m pos
    | val /= 0 = m
    | areas == [] = m
    | otherwise = setElem tag pos m
    where val = m ! pos
          areas = nub $ map (\p -> m ! p) $ neighbors m pos
          tag = head areas

mark :: Int -> Matrix Int -> (Int,Int) -> Matrix Int
mark tag m pos
    | mm == flowed = mm
    | otherwise = mark tag flowed pos
    where mm = setElem tag pos m
          flowed = foldl flow mm $ cells mm

areas :: Matrix Int -> Int -> [(Int,Int)] -> (Int, Matrix Int)
areas m c [] = (c,m)
areas m count (p:ps)
    | val /= 0 = areas m count ps
    | otherwise = areas tagged tag ps
    where val = m ! p
          tag = count + 1
          tagged = mark tag m p

run str = map show [length $ filter (>0) hashes, tags]
    where hashes = concatMap (\s -> binhash $ str ++ "-" ++ show s) [0..127]
          m = fromList 128 128 $ map pred hashes
          (tags,mm) = areas m 0 $ cells m
