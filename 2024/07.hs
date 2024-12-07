process as = map (show.sum) (map eval [[sum,product], [sum,product,concat]])
    where eval fns = map (\(t,(a:bs)) -> solve fns t bs a) as
          concat (a:b:_) = read (show a ++ show b)

solve :: [([Int]->Int)] -> Int -> [Int] -> Int -> Int
solve _ tot [] n
    | n == tot = n
    | otherwise = 0
solve fns tot (a:bs) n = maximum (map (\f -> solve fns tot bs (f [n,a])) fns)

parse :: String -> (Int, [Int])
parse s = (read $ init tot, map read parts)
    where (tot:parts) = words s

main :: IO ()
main = interact (unlines . process . map parse . lines)
