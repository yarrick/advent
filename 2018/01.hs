import qualified Data.Map as M

parse :: String -> (Int -> Int)
parse (c:num)
    | c == '+' = (+n)
    | c == '-' = (+(-n))
    where n = read num

firstdup :: M.Map Int Int -> [Int] -> Int
firstdup seen (x:xs)
    | M.member x seen = x
    | otherwise = firstdup (M.insert x 1 seen) xs

process :: [(Int -> Int)] -> [String]
process ops = map show [foldl apply 0 ops,
                        firstdup M.empty $ scanl apply 0 $ cycle ops ]
    where apply n fn = fn n

main :: IO ()
main = interact (unlines . process . map parse . lines)
