import System.IO
import Data.List

run file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    putStr $ unlines $ process $ lines contents
    hClose handle

parse :: String -> (String,String)
parse str = (head w, last w)
  where w = words str

process :: [String] -> [String]
process txt = map show [length $ nub $ calibrate (last txt) xforms,
  deconstruct (last txt) xforms]
  where xforms = map parse $ takeWhile (\l -> length l > 0) txt

calibrate :: String -> [(String,String)] -> [String]
calibrate base xform = concat $ map (apply base) xform

apply :: String -> (String,String) -> [String]
apply base xform = concat $ map (attempt base xform) $ take (length base) [0..]

attempt :: String -> (String,String) -> Int -> [String]
attempt base (from,to) idx
  | take (length from) (drop idx base) /= from = []
  | otherwise = [take idx base ++ to ++ drop (idx + length from) base]

-- part 2

deconstruct :: String -> [(String,String)] -> Int
deconstruct str xforms = work 1 str xf
  where xf = sortBy (\(a,b) (c,d) -> compare (length d) (length b)) xforms

work n str xf
  | newstr == "e" = n
  | otherwise = work (n+1) newstr xf
  where newstr = revxform str xf

revxform str [] = str
revxform str (x:xs)
  | str /= newstr = newstr
  | otherwise = revxform str xs
  where newstr = replace str x

replace :: String -> (String,String) -> String
replace ss (to,from)
 | lf > length ss = ss
 | take lf ss == from = to ++ drop lf ss
 | otherwise = head ss : replace (tail ss) (to,from)
 where lf = length from
