parse :: String -> (Int -> Int)
parse (c:num)
  | c == '+' = (+n)
  | c == '-' = (+(-n))
  where n = read num

firstdup :: [Int] -> [Int] -> Int
firstdup seen (x:xs)
  | elem x seen = x
  | otherwise = firstdup (x:seen) xs

process :: [String] -> [String]
process rows = [show $ foldl apply 0 ops,
                show $ firstdup [] $ scanl apply 0 $ cycle ops ]
  where apply n fn = fn n
        ops = map parse rows

main :: IO ()
main = interact (unlines . process . lines)
