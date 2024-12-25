import Data.List

process sms = [show $ length $ filter id [fits l k | l <- locks, k <- keys]]
    where (locks,keys) = partition (\p -> '.' == (head $ head p)) sms
          depth = length $ head $ head sms
          fits l k = all (<=depth) $ zipWith (+) (heights l) (heights k)
               where heights v = map (length.filter ('#'==)) v

parse m
    | rest == [] = [transpose pattern]
    | otherwise = transpose pattern : parse (tail rest)
    where (pattern, rest) = break (""==) m

main :: IO ()
main = interact (unlines . process . parse . lines)
