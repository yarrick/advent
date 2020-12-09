import Knot

part1 :: [Int] -> Int
part1 twists = product $ take 2 $ run twists

part2 :: String -> String
part2 = knot
