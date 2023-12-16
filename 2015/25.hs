import Data.Char

pos :: Integer -> Integer -> Integer
pos row col = foldl (+) colstart $ takex row col
    where
        takex num start = take (fromInteger (num - 1)) [start..]
        colstart = foldl (+) 1 $ takex col 2

calc :: Integer -> Integer -> Integer
calc row col = (20151125 * ((252533 ^ num) `mod` 33554393)) `mod` 33554393
    where num = (pos row col) - 1

process (row:_) = [show $ calc a b]
    where (a:b:_) = map read $ filter (""/=) $ map (filter isDigit) $ words row

main :: IO ()
main = interact (unlines . process . lines)
