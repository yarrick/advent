process :: [[Int]] -> [String]
process rows = map (show.length.(\f -> filter f rows)) [valid 0, valid2]
    where valid n r = steps (succ) n r || steps (pred) n r
          valid2 r = valid 1 r || valid 0 (tail r)

steps :: (Int -> Int) -> Int -> [Int] -> Bool
steps _ skips (a:[]) = skips >= 0
steps op skips (a:b:cs)
    | elem b (tail $ take 4 $ iterate op a) =
        (steps op skips (b:cs)) || (steps op (pred skips) (a:cs))
    | otherwise = steps op (pred skips) (a:cs)

parse r = map read $ words r

main :: IO ()
main = interact (unlines . process . map parse . lines)
