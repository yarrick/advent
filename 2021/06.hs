import Data.List

build :: [Int] -> [Int]
build s = merge sets [0..8]
    where sets = map (\sl -> (head sl, length sl)) $ group $ sort s
          merge [] bs = replicate (length bs) 0
          merge ((v,len):vs) (b:bs)
            | v == b = len : merge vs bs
            | otherwise = 0 : merge ((v,len):vs) bs

age :: [Int] -> [Int]
age (f:fs) = zipWith (+) born (fs ++ [0])
    where born = (replicate 6 0) ++ [f,0,f]

run :: [Int] -> [Int]
run fs = [breed !! 80, breed !! 256]
    where breed = map sum $ iterate age $ build fs

process :: [String] -> [String]
process (row:_) = map show $ run nums
    where nums = read $ "[" ++ row ++ "]"

main :: IO ()
main = interact (unlines . process . lines)
