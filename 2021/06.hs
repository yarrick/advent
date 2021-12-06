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

-- Add brackets around input: 3,4,3,1,2 -> [3,4,3,1,2]
run :: [Int] -> (Int, Int)
run fs = (breed !! 80, breed !! 256)
    where breed = map sum $ iterate age $ build fs
