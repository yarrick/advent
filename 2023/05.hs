import Data.Char
import Data.List

process :: ([Int], [[[Int]]]) -> [String]
process (seed,nums) = map show [solve xlate seed, fst $ solve xlate2 $ exp seed]
    where solve fn sd = minimum $ foldl fn sd nums
          exp [] = []
          exp (a:b:cs) = (a,a+b-1) : exp cs

xlate :: [Int] -> [[Int]] -> [Int]
xlate [] _ = []
xlate inp [] = inp
xlate inp ((dstart:sstart:len:[]):xs) = map conv match ++ xlate out xs
    where (match,out) = partition (\i -> i >= sstart && i < (sstart + len)) inp
          conv x = x - sstart + dstart

xlate2 :: [(Int,Int)] -> [[Int]] -> [(Int,Int)]
xlate2 [] _ = []
xlate2 inp [] = inp
xlate2 ((start,end):is) xl@((dstart:sstart:len:[]):xs)
    | start > last || end < sstart = xlate2 [(start,end)] xs ++ xlate2 is xl
    | start >= sstart && end <= last = conv (start, end) : xlate2 is xl
    | start >= sstart && end > last = conv (start, last) : xlate2 ((after,end):is) xl
    | start < sstart && end <= last = conv (sstart, end) : xlate2 ((start,before):is) xl
    | start < sstart && end > last = conv (sstart, last) : xlate2 ((start,before):(after,end):is) xl
    where before = sstart - 1
          last = sstart + len - 1
          after = sstart + len
          conv (x,y) = (x - sstart + dstart, y - sstart + dstart)

parse :: [String] -> ([Int], [[[Int]]])
parse (r:_:rs) = (map read $ tail $ words r, nrows [] rs)
    where nrows a [] = [a]
          nrows a (s:ss)
            | length s == 0 = a : (nrows [] ss)
            | isDigit (head s) = nrows (a++[map read $ words s]) ss
            | otherwise = nrows a ss

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

