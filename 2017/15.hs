import Data.Bits
import Data.Char

mulcycle :: Int -> Int -> Int
mulcycle fact start = next
    where next = mod (start * fact) 2147483647

same16 :: [(Int,Int)] -> Int
same16 [] = 0
same16 ((a,b):cs)
    | same = 1 + same16 cs
    | otherwise = same16 cs
    where same = (a .&. 0xFFFF) == (b .&. 0xFFFF)

andZero :: Int -> Int -> Bool
andZero mask n = (n .&. mask) == 0

aGen start = tail $ iterate (mulcycle 16807) start
bGen start = tail $ iterate (mulcycle 48271) start

run astart bstart = same16 $ take 40000000 $ zip (aGen astart) (bGen bstart)

run2 astart bstart = same16 $ take 5000000 $ zip avals bvals
    where avals = filter (andZero 3) $ aGen astart
          bvals = filter (andZero 7) $ bGen bstart

process :: [String] -> [String]
process rows = map show [run anum bnum, run2 anum bnum]
    where (anum:bnum:_) = map (read.filter isDigit) rows

main :: IO ()
main = interact (unlines . process . lines)
