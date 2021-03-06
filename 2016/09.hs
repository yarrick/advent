
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
main = interact (unlines . process2 . lines)

-- part 2

expand2 :: String -> Int
expand2 [] = 0
expand2 ('(':bb) = expand2 expanded + expand2 nstr
  where (exp,tt) = break (')'==) bb
        (count,rep) = parse exp
        repstr = take count $ tail tt
        nstr = drop count $ tail tt
        expanded = take (count*rep) (cycle repstr)
expand2 (_:bb) = 1 + expand2 bb

process2 :: [String] -> [String]
process2 rows = map (show . expand2) rows

