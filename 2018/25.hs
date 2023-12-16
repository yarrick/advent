import Data.List

parse :: String -> [Int]
parse [] = []
parse str = read next : parse (drop (length next+1) str)
    where next = takeWhile (\c -> c /= ',') str

nearby :: [Int] -> [Int] -> Bool
nearby a b = dist <= 3
    where dist = sum $ map abs $ zipWith (-) a b

build :: [[[Int]]] -> [Int] -> [[[Int]]]
build cst star
    | length matches > 0 = (star : concat matches) : other
    | otherwise = [star] : cst
    where (matches,other) = partition (any (nearby star)) cst

process :: [[Int]]  -> [String]
process stars = [show $ length $ foldl build [] stars]

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
