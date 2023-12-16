import Data.List

diffone :: (Int,String) -> String -> String -> String
diffone (_,s) [] [] = s
diffone (diffs,s) (a:as) (b:bs)
  | diffs > 1 = []
  | diffs == 1 && a /= b = []
  | a /= b = diffone (diffs+1,s) as bs
  | otherwise = diffone (diffs,s++[a]) as bs

differ :: [String] -> [String]
differ (s:ss) = map (diffone (0,"") s) ss ++ differ ss

process rows = [show $ length (filter id $ map (haslen 2) cdata) * length (filter id $ map (haslen 3) cdata),
                head $ filter (\x -> length x > 0) $ differ rows]
  where cdata = map (filter (\(_,len) -> len >= 2 && len <= 3).map (\cs -> (cs,length cs)).group.sort) rows
        haslen len row = length (filter (\(_,l) -> l == len) row) > 0

main :: IO ()
main = interact (unlines . process . lines)
