import Data.Char
import Data.List

type Point = (Int, Int)

draw :: (Point, Point) -> [Point]
draw ((xa,ya), (xb,yb))
    | xa == xb && ya <= yb = [ (xa,n) | n <- [ya..yb] ]
    | xa == xb = [ (xa,n) | n <- [yb..ya] ]
    | ya == yb && xa <= xb = [ (n,ya) | n <- [xa..xb] ]
    | ya == yb = [ (n,ya) | n <- [xb..xa] ]
    | xa <= xb && ya <= yb = zip [xa..xb] [ya..yb]
    | xa <= xb = zip [xa..xb] (reverse [yb..ya])
    | xa > xb && ya <= yb = zip (reverse [xb..xa]) [ya..yb]
    | xa > xb = zip (reverse [xb..xa]) (reverse [yb..ya])

parse :: String -> (Point, Point)
parse s = (read a, read b)
    where (a:_:b:[]) = map (\w -> "(" ++ w ++ ")") $ words s

process :: [String] -> [String]
process rows = map (show . solve) [ filter hvline vents, vents ]
    where vents = map parse rows
          solve = length . filter (\n -> length n > 1) . group . sort . concat . (map draw)
          hvline (p1, p2) = fst p1 == fst p2 || snd p1 == snd p2

main :: IO ()
main = interact (unlines . process . lines)

