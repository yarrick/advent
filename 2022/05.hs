import Data.Char
import Data.List

crane :: [String] -> (String -> String) -> (Int,Int,Int) -> (Int,String) -> String
crane stacks rev (num,from,to) (col,s)
    | col == from = drop num s
    | col == to = (rev $ take num (stacks !! from)) ++ s
    | otherwise = s

process :: ([String],[(Int,Int,Int)]) -> [String]
process (t,ms) = map (show.map head.(\f -> foldl (move f) t ms)) [reverse, id]
    where move rev stacks m = map (crane stacks rev m) $ zip [0..] stacks

parse :: [String] -> ([String],[(Int,Int,Int)])
parse rows = (map stripnum $ transpose $ map (map (filter isAlphaNum)) towers,
              map (zerobase.instr) moves)
    where take4 [] = []
          take4 s = take 4 s : take4 (drop 4 s)
          towers = map take4 $ reverse $ takeWhile (/="") rows
          moves = filter (\r -> length r > 0 && head r == 'm') rows
          instr m = map read $ filter (isDigit.head) $ words m
          zerobase (num:from:to:[]) = (num,from-1,to-1)
          stripnum ss = reverse $ map head $ filter (\x -> length x > 0) $ drop 1 ss

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

