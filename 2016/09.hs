
parse :: String -> (Int,Int)
parse str = (read a, read $ tail b)
  where (a,b) = break ('x'==) str

expand :: String -> Int
expand [] = 0
expand ('(':bb) = (rep * (length repstr)) + expand nstr
  where (exp,tt) = break (')'==) bb
        (count,rep) = parse exp
        repstr = take count $ tail tt
        nstr = drop count $ tail tt
expand (_:bb) = 1 + expand bb

process :: [String] -> [String]
process rows = map (show . expand) rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
