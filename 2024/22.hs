import Data.Bits
import qualified Data.Map.Strict as M

process :: [Int] -> [String]
process m = map show [sum $ map last secrets, maximum $ M.elems quads]
    where secrets = map (take 2001 . iterate secret) m
          trades = map ((trends M.empty) . diffs) secrets
          quads = foldr1 (M.unionWith (+)) trades

trends m (a:b:c:[]) = m
trends m ((a,_):(b,ba):(c,ca):(d,da):es) =
    trends (M.insertWith (\n o -> o) (a,b,c,d) da m) ((b,ba):(c,ca):(d,da):es)

diffs (a:[]) = []
diffs (a:b:cs) = (diff a b) : diffs (b:cs)
    where diff x y = ((mod y 10) - (mod x 10), mod y 10)

secret n = step (*2048) $ step (`div` 32) $ step (*64) n
    where step fn num = mod (xor (fn num) num) 16777216

main :: IO ()
main = interact (unlines . process . map read . lines)
