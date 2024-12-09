import Data.Char

-- length, and id (for blocks)
data Unit  = Empty Int | Block Int Int deriving (Eq,Show)

process ts = map (show.checksum 0) [ compact $ build 0 ts, part2 ]
    where part2 = reverse $ defrag $ reverse $ build 0 ts

build :: Int -> [Int] -> [Unit]
build _ [] = []
build blockid (a:0:cs) = (Block a blockid) : build (succ blockid) cs
build blockid (a:b:cs) = (Block a blockid) : (Empty b) : build (succ blockid) cs
build blockid (a:[]) = [Block a blockid]

isEmpty (Block n i) = False
isEmpty (Empty n) = True

compact :: [Unit] -> [Unit]
compact (a:[]) = (a:[])
compact ((Block l b):bs) = (Block l b) : compact bs
compact ((Empty n):bs)
    | isEmpty (last bs) = compact ((Empty n):(init bs))
    | lb < n = (Block lb bb) : (compact $ (Empty (n-lb)):(init bs))
    | lb == n = (Block lb bb) : (compact (init bs))
    | otherwise = (Block n bb) : (compact (init bs ++ [Block (lb-n) bb]))
    where (Block lb bb) = last bs

defrag :: [Unit] -> [Unit]
defrag [] = []
defrag (Empty n:us) = (Empty n) : defrag us
defrag (Block n bb:us)
    | length fits > 0 && el > n = (Empty n) : (defrag $ reverse $ busy ++ [Block n bb, Empty (el-n)] ++ tail fits)
    | length fits > 0 = (Empty n) : (defrag $ reverse $ busy ++ [Block n bb] ++ tail fits)
    | otherwise = (Block n bb) : defrag us
    where space nl (Empty el) = el >= n
          space nl (Block _ _) = False
          (busy, fits) = break (space n) $ reverse us
          (Empty el) = head fits

checksum :: Int -> [Unit] -> Int
checksum _ [] = 0
checksum pos ((Empty len):us) = checksum (pos+len) us
checksum pos ((Block len blockid):us)
    | len == 0 = checksum pos us
    | otherwise = (pos * blockid) + checksum (succ pos) ((Block (pred len) blockid):us)

parse :: String -> [Int]
parse s = map (\r -> read [r]) $ filter isDigit s

main :: IO ()
main = interact (unlines . process . parse)
