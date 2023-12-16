import Data.Matrix
import Data.Maybe
import Data.List

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x > nrows m = fallback
  | y > ncols m = fallback
  | otherwise = getElem x y m

bugstep :: Matrix Char -> (Int,Int) -> Char
bugstep m (x,y)
  | self == '|' && length yards >= 3 = '#'
  | self == '|' = '|'
  | self == '#' && length yards >= 1 && length trees >= 1 = '#'
  | self == '#' = '.'
  | self == '.' && length trees >= 3 = '|'
  | self == '.' = '.'
  where
    self = getxy m (x,y) '.'
    peers = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
    trees = filter ('|'==) $ map (\pos -> getxy m pos '.') peers
    yards = filter ('#'==) $ map (\pos -> getxy m pos '.') peers

nextmap :: Matrix Char -> Matrix Char
nextmap m = fromList (nrows m) (ncols m) $ map (bugstep m) $ [ (x,y) | x <- [1..(nrows m)], y <- [1..(ncols m)]]

score :: Matrix Char -> Int
score m = (length (filter ('#'==) content)) * (length (filter ('|'==) content))
  where content = toList m

findsame :: Eq a => [a] -> [a] -> [Int]
findsame log (e:es)
  | isJust p = (fromJust p) : findsame log es
  | otherwise = findsame (log++[e]) es
  where p = findIndex (e==) log

fastmap :: Matrix Char -> Int -> Matrix Char
fastmap m n
  | n < head cycles = maps !! n
  | otherwise = maps !! (head cycles + mod (n-(head cycles)) (looplen cycles))
  where maps = iterate nextmap m
        cycles = findsame [] $ iterate nextmap m
        looplen (a:as) = fst $ head $ dropWhile (\(_,v) -> v /= a) $ zip [1..] as

process rows = [show $ score $ (iterate nextmap m) !! 10, show $ score $ fastmap m 1000000000]
  where m = fromLists rows

main :: IO ()
main = interact (unlines . process . lines)
