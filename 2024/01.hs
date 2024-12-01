import Data.List

process :: [[Int]] -> [String]
process (a:b:[]) = map show [sum part1, part2 (group a) (group b)]
    where part1 = map abs $ zipWith (-) a b

part2 [] _ = 0
part2 _ [] = 0
part2 (a:as) (b:bs)
    | head a == head b = (length a) * (head a) * (length b) + part2 as bs
    | head a > head b = part2 (a:as) bs
    | head a < head b = part2 as (b:bs)

parse r = map read $ words r

main :: IO ()
main = interact (unlines . process . map sort . transpose . map parse . lines)
