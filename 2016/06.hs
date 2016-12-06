import Data.List

countAlpha :: [(Char,Int)] -> String -> [(Char,Int)]
countAlpha a [] = a
countAlpha count (a:bb) = countAlpha (addCount count a) bb

addCount :: [(Char,Int)] -> Char -> [(Char,Int)]
addCount [] c = [(c,1)]
addCount ((al,alcount):bb) c
 | al == c = (al,alcount+1) : bb
 | otherwise = (al,alcount) : addCount bb c

getTop :: [(Char,Int)] -> String
getTop count = [fst $ head count]

decode :: [String] -> ((Char,Int) -> (Char,Int) -> Ordering) -> String
decode rows cmp = foldl1 (++) $ map signal $ transpose rows
  where signal x = getTop $ sortBy cmp $ countAlpha [] x

process :: [String] -> [String]
process rows = [decode rows most, decode rows least]
  where most (ca,va) (cb,vb) = compare vb va
        least (ca,va) (cb,vb) = compare va vb

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
