import Data.List

process :: [(Int,Int)] -> [String]
process moves = map (show.sum) [concatMap (nums invalid) moves, concatMap (nums i2) moves]
    where nums fn (a,b) = map read $ filter fn $ map show [(a)..(b)]
          invalid digits = even len && take halflen digits == drop halflen digits
               where len = length digits
                     halflen = div len 2
          i2 digits = any (digits==) $ map cand $ filter oklen all
               where all = drop 1 $ inits digits
                     oklen s = mod (length digits) (length s) == 0 && div (length digits) (length s) > 1
                     cand s = concat $ take (div (length digits) (length s)) $ cycle [s]

parse p
    | elem ',' p = (get curr) : parse (drop 1 rest)
    | otherwise = [get p]
    where (curr, rest) = break (','==) p
          get rng = (read a, read $ drop 1 b)
            where (a,b) = break ('-'==) rng

main :: IO ()
main = interact (unlines . process . concatMap parse . lines)
