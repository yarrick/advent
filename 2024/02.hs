process :: [[Int]] -> [String]
process rows = map (show.length.(\f -> filter f rows)) [valid, valid2]
    where valid r = steps (succ) r || steps (pred) r
          valid2 r = any valid $ r : map (\p -> take p r ++ drop (p+1) r) [0..length r-1]

steps :: (Int -> Int) -> [Int] -> Bool
steps _ (a:[]) = True
steps op (a:b:cs)
    | elem b (tail $ take 4 $ iterate op a) = (steps op (b:cs))
    | otherwise = False

parse r = map read $ words r

main :: IO ()
main = interact (unlines . process . map parse . lines)
