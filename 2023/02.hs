import Data.Char
import Data.List
import Data.Maybe

-- red, green, blue
type Grab = (Int, Int, Int)

process :: [(Int, Grab)] -> [String]
process games = map (show . sum . (\f -> map f games)) [play, pwr]
    where pwr (_, (r,g,b)) = product [r,g,b]

play :: (Int, Grab) -> Int
play (num, (r,g,b))
    | r <= 12 && g <= 13 && b <= 14 = num
    | otherwise = 0

parse :: String -> (Int, Grab)
parse r = (read $ filter isDigit $ head gs, foldl grab (0,0,0) ps)
    where (gs:ps) = map (chunk ",") $ chunk ";:" r

grab :: Grab -> [String] -> Grab
grab g [] = g
grab (r,g,b) (s:ss)
    | color == "red" = grab (max r num,g,b) ss
    | color == "green" = grab (r,max g num,b) ss
    | color == "blue" = grab (r,g,max b num) ss
    where num = read $ filter isDigit s
          color = filter isAlpha s

chunk :: String -> String -> [String]
chunk _ [] = []
chunk sep s
    | next == Nothing = [s]
    | otherwise = (take pos s) : chunk sep (drop (succ pos) s)
    where next = findIndex (\c -> elem c sep) s
          pos = fromMaybe 0 next

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
