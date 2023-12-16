import Data.List
import Data.Char

make :: Int -> Int -> [[Int]]
make x y = replicate y $ replicate x 0

rotate :: [a] -> Int -> [a]
rotate list n = post ++ pre
  where (pre,post) = splitAt (length list - n) list

rect :: Int -> Int -> [[Int]] -> [[Int]]
rect _ 0 rows = rows
rect _ _ [] = []
rect x y (r:rows) = ((replicate x 1) ++ (drop x r)) : rect x (y-1) rows

rotrow :: Int -> Int -> [[Int]] -> [[Int]]
rotrow 0 n (r:rows) = (rotate r n) : rows
rotrow y n (r:rows) = r : rotrow (y-1) n rows

rotcol :: Int -> Int -> [[Int]] -> [[Int]]
rotcol x n rows = transpose $ rotrow x n $ transpose rows

val :: [[Int]] -> Int
val rows = sum $ map sum rows

data Op = Rect Int Int | RotRow Int Int | RotCol Int Int deriving (Show)

getRect :: String -> Op
getRect str = Rect (read x) (read $ tail y)
  where (x,y) = span isDigit str

parse :: String -> Op
parse cmd
 | head args == "rect" = getRect $ args !! 1
 | args !! 1 == "row" = RotRow y n
 | otherwise = RotCol y n
  where args = words cmd
        y = read $ drop 2 $ args !! 2
        n = read $ args !! 4

perform :: [[Int]] -> Op -> [[Int]]
perform r (Rect x y) = rect x y r
perform r (RotRow y n) = rotrow y n r
perform r (RotCol x n) = rotcol x n r

translate :: Int -> Char
translate 1 = '#'
translate 0 = ' '

process :: [String] -> [String]
process rows = (show $ sum $ map sum result) : map (map translate) result
  where result = foldl perform (make 50 6) $ map parse rows

main :: IO ()
main = interact (unlines . process . lines)
