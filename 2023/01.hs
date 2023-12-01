import Data.Char

process :: [String] -> [String]
process rows = map (show.sum.digits) [rows, map conv rows]
    where digits inp = map (read . pick . filter isDigit) inp
          pick s = head s : last s : []

conv :: String -> String
conv [] = []
conv s
    | take 3 s == "one" = '1' : conv (tail s)
    | take 3 s == "two" = '2' : conv (tail s)
    | take 5 s == "three" = '3' : conv (tail s)
    | take 4 s == "four" = '4' : conv (tail s)
    | take 4 s == "five" = '5' : conv (tail s)
    | take 3 s == "six" = '6' : conv (tail s)
    | take 5 s == "seven" = '7' : conv (tail s)
    | take 5 s == "eight" = '8' : conv (tail s)
    | take 4 s == "nine" = '9' : conv (tail s)
    | otherwise = (head s) : conv (tail s)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

