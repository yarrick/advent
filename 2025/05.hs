import Data.List

merge [(a,b)] = (b - a + 1)
merge ((a,b):(c,d):fs)
    | c <= b+1 = merge $ (a, maximum [b,d]):fs
    | otherwise = merge [(a,b)] + merge ((c,d):fs)

process (fresh,stock) = map show [length $ filter good stock, merge $ sort fresh]
    where inside n (a,b) = n >= a && n <= b
          good n = any (inside n) fresh

parse :: [String] -> ([(Int,Int)], [Int])
parse ss = (map range rngs, map read $ drop 1 rest)
    where (rngs, rest) = break (""==) ss
          range rs = (read from, read $ drop 1 to)
            where (from,to) = break ('-'==) rs

main :: IO ()
main = interact (unlines . process . parse . lines)
