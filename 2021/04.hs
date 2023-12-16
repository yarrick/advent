import Data.Char
import Data.List

type Board = [[Int]]

bingo :: [Board] -> [Int] -> Int -> [([Board], [Int])]
bingo [] _ _ = []
bingo bs guesses num
    | winners == [] = bingo bs guesses (succ num)
    | otherwise = (winners, take num guesses) : bingo losers guesses (succ num)
    where winners = filter (winner $ take num guesses) bs
          losers = filter (\b -> notElem b winners) bs

score :: ([Board], [Int]) -> Int  -- only scores first board in list.
score (bs, gs) = last gs * unmarked
    where unmarked = sum $ filter (\n -> notElem n gs) (concat $ head bs)

winner :: [Int] -> Board -> Bool
winner guesses rows = any rowwin $ rows ++ transpose rows
    where rowwin rr = all (\r -> elem r guesses) rr

parse :: [String] -> ([Int], [Board])
parse (g:bs) = (map read $ filter (/=",") $ groupBy isnum g, chunk bs)
    where isnum a b = isDigit a == isDigit b
          chunk [] = []
          chunk rs = (map (map read.words) $ tail $ take 6 rs) : chunk (drop 6 rs)

process :: [String] -> [String]
process rows = map (show.score) [head result, last result]
    where (guesses,boards) = parse rows
          result = bingo boards guesses 1

main :: IO ()
main = interact (unlines . process . lines)

