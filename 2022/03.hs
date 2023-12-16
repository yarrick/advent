import Data.Char
import Data.List

part1 :: String -> String
part1 s = intersect (take l s) (drop l s)
    where l = div (length s) 2

part2 :: [String] -> [String]
part2 [] = []
part2 (a:b:c:cs) = (intersect a (intersect b c)) : part2 cs

prio :: Char -> Int
prio c
    | c >= 'a' && c <= 'z' = 1 + (ord c) - (ord 'a')
    | otherwise = 27 + (ord c) - (ord 'A')

process :: [String] -> [String]
process rows = map (show.sum.map (prio.head)) [map part1 rows, part2 rows]

main :: IO ()
main = interact (unlines . process . lines)
