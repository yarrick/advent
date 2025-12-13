import Data.Char
import Data.List

pairings (a:[]) = []
pairings (a:bs) = [ [a,b] | b <- bs] ++ pairings bs

area ps = range xs * range ys
    where (xs,ys) = unzip ps
          range rs = 1 + maximum rs - minimum rs

process :: [(Int,Int)] -> [String]
process ps = [show $ last $ sort $ map area $ pairings ps]

parse s = read $ "(" ++ s ++ ")"

main :: IO ()
main = interact (unlines . process . map parse . lines)
