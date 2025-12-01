process :: [(Char, Int)] -> [String]
process moves = map (show.zeros) [moves, concatMap expand moves]
    where move p (d, len)
            | d == 'L' = mod (100 + p - len) 100
            | otherwise = mod (p + len) 100
          zeros ms = length $ filter (==0) $ scanl move 50 ms
          expand (d,len) = take len $ cycle [(d,1)]

parse (d:ds) = (d, read ds)

main :: IO ()
main = interact (unlines . process . map parse . lines)
