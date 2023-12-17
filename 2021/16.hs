
bin :: Char -> [Int]
bin '0' = [0,0,0,0]
bin '1' = [0,0,0,1]
bin '2' = [0,0,1,0]
bin '3' = [0,0,1,1]
bin '4' = [0,1,0,0]
bin '5' = [0,1,0,1]
bin '6' = [0,1,1,0]
bin '7' = [0,1,1,1]
bin '8' = [1,0,0,0]
bin '9' = [1,0,0,1]
bin 'A' = [1,0,1,0]
bin 'B' = [1,0,1,1]
bin 'C' = [1,1,0,0]
bin 'D' = [1,1,0,1]
bin 'E' = [1,1,1,0]
bin 'F' = [1,1,1,1]

deci :: [Int] -> Int
deci n = foldl (\a b -> 2*a+b) 0 n

data Content = Literal Int | Operator Int [Pkt] deriving (Eq,Show)
type Pkt = (Int,Content)

int4 :: ([Int],[Int]) -> ([Int],[Int])
int4 (p,(n:ns))
        | n == 1 = int4 (p++num, rest)
        | otherwise = (p++num, rest)
        where (num,rest) = splitAt 4 ns

mparse :: Int -> [Int] -> [Pkt] -> ([Pkt],[Int])
mparse 0 rest pkts = (pkts,rest)
mparse n bits pkts = mparse (n-1) end (pkts++[pkt])
    where (pkt,end) = parse bits

subparse :: Int -> [Int] -> (Content,[Int])
subparse 4 bits = (Literal (deci num), rest)
    where (num, rest) = int4 ([], bits)
subparse n (0:bits) = (Operator n (descend sbits),end)
    where (sublen,rest) = splitAt 15 bits
          (sbits,end) = splitAt (deci sublen) rest
          descend [] = []
          descend bs = subpkg : descend next
            where (subpkg,next) = parse bs
subparse n (1:bits) = (Operator n pkts, end)
    where (pcount,rest) = splitAt 11 bits
          (pkts, end) = mparse (deci pcount) rest []

parse :: [Int] -> (Pkt, [Int])
parse bits = ((deci $ take 3 hdr, content), end)
    where (hdr,rest) = splitAt 6 bits
          (content,end) = subparse (deci $ drop 3 hdr) rest

sumver :: Pkt -> Int
sumver (v, Literal _) = v
sumver (v, Operator _ pkts) = v + sum (map sumver pkts)

pcmp :: (Int -> Int -> Bool) -> [Int] -> Int
pcmp op (a:b:[])
    | op a b = 1
    | otherwise = 0

eval :: Pkt -> Int
eval (_, Literal n) = n
eval (_, Operator oid pkts) = op $ map eval pkts
    where op = [sum, product, minimum, maximum, sum,
                pcmp (>), pcmp (<), pcmp (==)] !! oid

run :: String -> [Int]
run pktstr = [sumver pkt, eval pkt]
    where (pkt,trailer) = parse $ concatMap bin pktstr

process :: [String] -> [String]
process (row:_) = map show $ run row

main :: IO ()
main = interact (unlines . process . lines)
