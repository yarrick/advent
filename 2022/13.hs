import Data.Char
import Data.List

data Nested = N [Nested] | I Int deriving (Read, Show, Eq)

ordered :: Nested -> Nested -> Ordering
ordered (N []) (N []) = EQ
ordered (N []) (N _) = LT
ordered (N _) (N []) = GT
ordered (N as) (I b) = ordered (N as) (N [I b])
ordered (I a) (N bs) = ordered (N [I a]) (N bs)
ordered (N ((I a):as)) (N ((I b):bs))
    | a < b = LT
    | a > b = GT
    | a == b = ordered (N as) (N bs)
ordered (N (a:as)) (N (b:bs))
    | order == EQ = ordered (N as) (N bs)
    | otherwise = order
    where order = ordered a b


process :: [String] -> [String]
process rows = map show [sum $ map fst $ filter ((==LT).snd) pairs,
                         product $ map fst $ filter divider $ zip [1..] signals]
    where pairs = zip [1..] $ map (\(a,b) -> ordered a b) $ chunk rows
          dividers = [N [N [I 2]], N [N [I 6]]]
          divider (_,d) = elem d dividers
          signals = sortBy ordered $ dividers ++ (map read $ filter (/="") rows)

chunk :: [String] -> [(Nested,Nested)]
chunk [] = []
chunk (a:b:cs) = (read a, read b) : chunk (drop 1 cs)

-- Make it 'read'-able using nested data type
pad :: String -> String
pad [] = []
pad ('[':ss) = 'N':' ':'[' : pad ss
pad ss
    | isDigit (head ss) = 'I':' ':(takeWhile isDigit ss) ++ (pad (dropWhile isDigit ss))
    | otherwise = (head ss):(pad (tail ss))

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map pad . lines)
