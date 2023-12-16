import Data.List

chunk :: [String] -> [[Int]]
chunk ss = map (map read) $ filter (/=[""]) $ groupBy (\x y -> length x > 0 && length y > 0) ss

process :: [String] -> [String]
process rows = map show [head elves, sum $ take 3 elves]
    where elves = reverse $ sort $ map sum $ chunk rows

main :: IO ()
main = interact (unlines . process . lines)
