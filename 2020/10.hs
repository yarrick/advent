import Data.List

diffs :: [Integer] -> [Integer]
diffs [a] = []
diffs (a:b:cs) = (b-a) : diffs (b:cs)

ways [] = 0
ways [1] = 1
ways [1,1] = 2
ways [1,1,1] = 4
ways n
    | elem 3 n = 1
    | otherwise = sum $ map ways $ take (length n) $ inits n

process :: [Integer] -> [String]
process nums = map (show . product) [map length $ group $ sort steps,
                                     map ways $ group steps]
    where steps = diffs $ [0] ++ sort nums ++ [3+maximum nums]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map read) . lines)

