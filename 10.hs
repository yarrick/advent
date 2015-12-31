import Data.List (group)

count :: [Int] -> [Int]
count x = (length x) : [head x]

flatten :: [[t]] -> [t]
flatten x = foldl (++) [] x

step :: [Int] -> [Int]
step nbrs = flatten $ map count $ group nbrs

looksay :: String -> String
looksay str = flatten $ map show $ step nbrs
	where nbrs = map read $ map (:[]) str

parse :: String -> [Int]
parse str = map read $ map (:[]) str

run :: String -> Int -> [Int]
run str num = take num $ map length $ iterate step $ parse str
