import Data.List
import Data.Maybe

data Move = Spin Int | Exchange Int Int | Swap Char Char deriving (Eq,Show)

parse :: String -> [Move]
parse [] = []
parse (',':cs) = parse cs
parse (c:cs)
    | c == 's' = (Spin (read args)) : parse next
    | c == 'x' = (Exchange (read a) (read $ tail b)) : parse next
    | c == 'p' = (Swap (head a) (head $ tail b)) : parse next
    where args = takeWhile (/=',') cs
          (a,b) = break ('/'==) args
          next = dropWhile (/=',') cs

step :: String -> Move -> String
step line (Spin s) = drop len line ++ take len line
    where len = (length line) - s
step line (Exchange a b) = map (\x -> line !! (newpos x)) [0..length line-1]
    where newpos n
            | n == a = b
            | n == b = a
            | otherwise = n
step line (Swap a b) = step line (Exchange (get a) (get b))
    where get c = fromJust $ elemIndex c line

dance :: String -> [Move] -> String
dance line moves = foldl step line moves

loop :: [Move] -> String -> [String] -> [String]
loop moves line old
    | elem next old = old
    | otherwise = loop moves next (old ++ [next])
    where next = dance line moves

process :: [Move] -> [String]
process moves = map show [head c, c !! (mod 1000000000 (length c) - 1)]
    where c = loop moves ['a'..'p'] []

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . concatMap parse . lines)

