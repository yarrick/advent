import Data.Char
import Data.List

process :: ([Int], [[[Int]]]) -> [String]
process (seed,nums) = map solve [map (\n -> (n,n)) seed, exp seed]
    where solve sd = show $ fst $ minimum $ foldl xlate sd nums
          exp [] = []
          exp (a:b:cs) = (a,a+b-1) : exp cs

xlate :: [(Int,Int)] -> [[Int]] -> [(Int,Int)]
xlate [] _ = []
xlate inp [] = inp
xlate ((start,end):is) xl@((dstart:sstart:len:[]):xs)
    | start > last || end < sstart = xlate [(start,end)] xs ++ xlate is xl
    | start >= sstart && end <= last = conv (start, end) : xlate is xl
    | start >= sstart && end > last = conv (start, last) : xlate ((after,end):is) xl
    | start < sstart && end <= last = conv (sstart, end) : xlate ((start,before):is) xl
    | start < sstart && end > last = conv (sstart, last) : xlate ((start,before):(after,end):is) xl
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

