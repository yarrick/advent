import Data.Char
import Data.List

parse :: String -> ([(Char,Int)], Int, String, String)
parse str = (sortBy cmp acount, num, csum, word)
  where (acount,rest) = countAlpha ([],[]) str
        num = read $ takeWhile isDigit rest
        csum = takeWhile isAlpha $ tail $ dropWhile isDigit rest
        word = takeWhile (not . isDigit) str

cmp :: (Char,Int) -> (Char,Int) -> Ordering
cmp (ca,va) (cb,vb)
 | va == vb = compare ca cb
 | otherwise = compare vb va

countAlpha :: ([(Char,Int)],String) -> String -> ([(Char,Int)],String)
countAlpha a [] = a
countAlpha (count,rest) (a:bb)
 | isDigit a = (count,a:bb)
 | a == '-' = countAlpha (count,rest) bb
 | otherwise = countAlpha (addCount count a,rest) bb

addCount :: [(Char,Int)] -> Char -> [(Char,Int)]
addCount [] c = [(c,1)]
addCount ((al,alcount):bb) c
 | al == c = (al,alcount+1) : bb
 | otherwise = (al,alcount) : addCount bb c

check :: ([(Char,Int)], Int, String, String) -> (Int, String)
check (acount, val, csum, word)
 | str == csum = (val,word)
 | otherwise = (0,word)
  where str = take 5 $ map fst acount

decrypt :: (Int,String) -> (Int,String)
decrypt (val,str) = (val,map (rotate (val `mod` 26)) str)

rotate :: Int -> Char -> Char
rotate _ '-' = ' '
rotate 0 c = c
rotate b 'z' = rotate (b-1) 'a'
rotate b c = rotate (b-1) $ succ c

isNorth :: (Int,String) -> Bool
isNorth (val,str) = isInfixOf "northpole" str

process :: [String] -> [String]
process rows = map (show.sum.map fst) [checked, filter isNorth $ map decrypt checked]
  where checked = filter (\a -> fst a > 0) $ map (check . parse) rows

main :: IO ()
main = interact (unlines . process . lines)
