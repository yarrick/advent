import Knot
import Data.Bits
import Text.Printf

part1 :: [Int] -> Int
part1 twists = product $ take 2 $ run [0..255] twists

part2 :: String -> String
part2 = knot
