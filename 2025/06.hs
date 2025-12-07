import Data.Char
import Data.List

process ms = map (show.calc) [parse ms, parse2 ms]
    where calc [] = 0
          calc ((nums, op):cs) = foldl1 op nums + calc cs

op "+" = (+)
op "*" = (*)

parse :: [String] -> [([Int], (Int -> Int -> Int))]
parse ss = map conv cols
    where cols = transpose $ map words ss
          conv col = (map read $ take (length col - 1) col, op $ last col)

parse2 :: [String] -> [([Int], (Int -> Int -> Int))]
parse2 ss = map conv cols
    where chunk [] = []
          chunk cs = first : chunk (drop 1 end)
            where (first, end) = break (""==) cs
          cols = chunk $ map (filter (not.isSpace)) $ transpose ss
          conv col = (map read $ map fst digits, op $ concat $ map snd digits)
             where digits = map (partition isDigit) col

main :: IO ()
main = interact (unlines . process . lines)
