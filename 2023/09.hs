import Data.List

process :: [String] -> [String]
process rows = map (show.sum.map extend) [digits, map reverse digits]
    where digits = map (map read.words) rows

extend :: [Int] -> Int
extend ss
    | length (group ss) == 1 = head ss
    | otherwise = last ss + extend (diff ss)
    where diff (a:[]) = []
          diff (a:b:cs) = (b-a) : diff (b:cs)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

