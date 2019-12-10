import System.IO
import Data.List

run file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    putStrLn $ process $ lines contents
    hClose handle

parse :: String -> (String,String)
parse str = (head w, last w)
  where w = words str

process :: [String] -> String
process txt = show $ length $ nub $ calibrate (last txt) $ map parse xforms
  where xforms = takeWhile (\l -> length l > 0) txt

calibrate :: String -> [(String,String)] -> [String]
calibrate base xform = concat $ map (apply base) xform

apply :: String -> (String,String) -> [String]
apply base xform = concat $ map (attempt base xform) $ take (length base) [0..]

attempt :: String -> (String,String) -> Int -> [String]
attempt base (from,to) idx
  | take (length from) (drop idx base) /= from = []
  | otherwise = [take idx base ++ to ++ drop (idx + length from) base]
