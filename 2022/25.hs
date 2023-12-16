num :: Char -> Int
num '-' = -1
num '=' = -2
num c = read [c]

snf :: Int -> Char
snf (-2) = '='
snf (-1) = '-'
snf n = "012" !! n

factors :: [Int]
factors = map (\c -> 5^c) [0..]

dec :: String -> Int
dec cs = sum $ map (\(f,n) -> f * n) pairs
    where fs = reverse $ take (length cs) factors
          pairs = zip fs $ map num cs

divs :: Int -> [Int] -> [Int]
divs n [1] = [n]
divs n (d:ds) = dv : divs rest ds
    where (dv,rest) = divMod n d

snaf n = map snf $ dropWhile (==0) $ reverse $ tune $ reverse $ divs n facts
    where smaller = takeWhile (<n) factors
          facts = reverse $ take (1 + length smaller) factors
          tune [a] = [a]
          tune (a:b:cs)
            | a > 2 = (a-5) : tune ((b+1):cs)
            | otherwise = a : tune (b:cs)

process :: [String] -> [String]
process rows = [snaf $ sum $ map dec rows]

main :: IO ()
main = interact (unlines . process . lines)
