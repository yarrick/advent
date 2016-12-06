import Data.List

countAlpha :: [(Char,Int)] -> String -> [(Char,Int)]
countAlpha a [] = a
countAlpha count (a:bb) = countAlpha (addCount count a) bb

addCount :: [(Char,Int)] -> Char -> [(Char,Int)]
addCount [] c = [(c,1)]
addCount ((al,alcount):bb) c
 | al == c = (al,alcount+1) : bb
 | otherwise = (al,alcount) : addCount bb c

decode :: ((Char,Int) -> (Char,Int) -> Ordering) -> String -> Char
decode cmp x = fst $ head $ sortBy cmp $ countAlpha [] x

process :: [String] -> [String]
process rows = [map (decode most) rows, map (decode least) rows]
  where most (ca,va) (cb,vb) = compare vb va
        least (ca,va) (cb,vb) = compare va vb

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . transpose . lines)
