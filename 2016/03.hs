import Data.List

check :: String -> Bool
check str = (last all) < (sum $ take 2 all)
  where all = sort $ parse str

parse :: String -> [Int]
parse str = map read $ words str

process :: [String] -> [String]
process rows = [show $ length $ filter (==True) $ map check rows]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
