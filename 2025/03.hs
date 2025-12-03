process :: [[Integer]] -> [String]
process batt = map (show.sum.compute) [2, 12]
    where compute n = map (foldl1 build . joltage n) batt
          build a b = (10 * a) + b

joltage c batt
    | c == 1 = [start]
    | otherwise = start : joltage (pred c) rest
    where start = maximum $ take (length batt - c + 1) batt
          rest = drop 1 $ dropWhile (start/=) batt

parse = map (\d -> read [d])

main :: IO ()
main = interact (unlines . process . map parse . lines)
