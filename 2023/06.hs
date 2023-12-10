process :: [String] -> [String]
process rows = map (show . run) [p1, [[t2],[d2]] ]
    where p1 = map (map read . tail . words) rows
          run (t:d:_) = product $ map race $ zip t d
          (t2:d2:[]) = map (read . concat . tail . words) rows

race :: (Int, Int) -> Int
race (t,d) = length $ filter (\k -> (t - k) * k > d) dists
    where dists = [1..t]

main :: IO ()
main = interact (unlines . process . lines)
