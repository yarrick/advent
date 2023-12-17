import Data.List

count :: Int -> Int -> Int
count from to = length $ filter valid [from..to]

valid :: Int -> Bool
valid a = twosame num && increasing num
  where num = show a

twosame :: [Char] -> Bool
twosame (a:b:cc)
  | a == b = True
  | otherwise = twosame (b:cc)
twosame (x:[]) = False

increasing :: [Char] -> Bool
increasing (a:b:cc)
  | a > b = False
  | otherwise = increasing (b:cc)
increasing (x:[]) = True

-- part 2

count2 :: Int -> Int -> Int
count2 from to = length $ filter valid2 [from..to]

valid2 :: Int -> Bool
valid2 a = twosame num && increasing num && pair num
  where num = show a

pair :: String -> Bool
pair num = elem 2 $ map length $ group num

process :: [String] -> [String]
process (row:_) = map show [count lo hi, count2 lo hi]
    where (a,b) = break ('-'==) row
          lo = read a
          hi = read $ tail b

main :: IO ()
main = interact (unlines . process . lines)
