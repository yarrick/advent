import Data.List
import Debug.Trace

process ms = [show $ sum $ map pushed ms]
    where pushed (p,b,j) = minimum $ concatMap (push (p,b,j)) (bitlists (length b))

push (pattern, buttons, _) presses
    | result 0 flips == pattern = [length $ filter id presses]
    | otherwise = []
    where start = take (length pattern) $ cycle [False]
          flips = sort $ concat $ map fst $ filter (\(b,p) -> p) $ zip buttons presses
          result n fs
            | n == (length pattern) = []
            | otherwise = (odd $ length match) : result (succ n) rest
            where (match,rest) = partition (n==) fs

bitlists n
    | n == 0 = [[]]
    | otherwise = (map (False:) prev) ++ (map (True:) prev)
        where prev = bitlists (pred n)

parse :: String -> ([Bool], [[Int]], [Int])
parse machine = (target, take (length nums -1) nums, last nums)
    where (flags:rest) = words machine
          target = map ('#'==) $ filter (\c -> elem c ".#") flags
          nums = map (read.replace) rest

replace [] = []
replace (c:cs)
    | c == '(' || c == '{' = '[' : replace cs
    | c == ')' || c == '}' = ']' : replace cs
    | otherwise = c : replace cs

main :: IO ()
main = interact (unlines . process . map parse . lines)
