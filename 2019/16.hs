import Control.DeepSeq

pick :: Int -> [Int] -> Int
pick _ [] = 0
pick n list = sum (take n (drop n list)) - sum (take n $ drop (3*n) list) + pick n (drop (4*n) list)

calc :: [Int] -> Int -> [Int]
calc num ph = [mod (abs $ pick ph (0:num)) 10]

phase :: [Int] -> [Int]
phase num = concatMap (calc num) [1..(length num)]

fft :: [Int] -> Int -> [[Int]]
fft num p = take (p+1) $ iterate phase num

run str = concat $ map show $ take 8 $ last $ fft (map (\c -> read [c]) str) 100

-- part 2

step :: Int -> [Int] -> [Int]
step _ [] = []
step s (x:xs) = mod (abs ss) 10 : step ss xs
  where ss = s + x

flow :: Int -> [Int] -> [Int]
flow n list
  | n == 100 = list
  | otherwise = deepseq nlist $ flow (n+1) nlist
  where nlist = step 0 list

run2 str = concat $ map show $ drop 101 $ take 109 $ reverse $ flow 0 newstr
   where offset = read $ take 7 str
         totlength = 10000 * length str
         newstr = reverse $ drop (offset-101) $ take totlength $ cycle $ map (\c -> read [c]) str

process :: [String] -> [String]
process (row:_) = [run row, run2 row]

main :: IO ()
main = interact (unlines . process . lines)
