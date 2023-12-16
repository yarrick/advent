import Data.List

frame :: String -> Int -> String -> Int
frame prev len (s:ss)
    | length (nub (s:prev)) == length (s:prev) = succ len
    | otherwise = frame (tail prev ++ [s]) (succ len) ss

run :: String -> [Int]
run s = [solve 3, solve 13]
    where solve sl = frame (take sl s) sl (drop sl s)

process :: [String] -> [String]
process (row:_) = map show $ run row

main :: IO ()
main = interact (unlines . process . lines)
