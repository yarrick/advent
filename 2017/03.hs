
ringlen 0 = 1
ringlen n = 8*n

ringstart 0 = (0,0)
ringstart n = (n, -(n-1))

up (x,y) = (x,y+1)
down (x,y) = (x,y-1)
right (x,y) = (x+1,y)
left (x,y) = (x-1,y)

ringop :: Int -> [(Int,Int) -> (Int,Int)]
ringop layer = tail $ concat $ map steps [up, left, down, right]
    where steps op = take (layer * 2) $ cycle [op]

walk pos ops = scanl (\n op -> op n) pos ops

ring :: Int -> [(Int,Int)]
ring 0 = [ringstart 0]
ring layer = walk (ringstart layer) $ ringop layer

ringpos :: Int -> Int -> (Int,Int)
ringpos layer n
    | n > len = ringpos (layer+1) (n-len)
    | otherwise = (ring layer) !! (n-1)
    where len = ringlen layer

part1 :: Int -> Int
part1 n = abs x + abs y
    where (x,y) = ringpos 0 n

nextscore :: Int -> [((Int,Int),Int)]
nextscore 1 = [((0,0),1)]
nextscore n = (pos, sum $ map (score prev) $ neighbors pos) : prev
    where prev = nextscore (n-1)
          pos = ringpos 0 n
          score ns pos = sum $ map snd $ filter (\(p,_) -> p == pos) ns
          neighbors pos = tail $ walk pos (right : ringop 1)

findgt :: Int -> Int -> Int
findgt n p
    | sc > n = sc
    | otherwise = findgt n (p+1)
    where sc = snd $ head $ nextscore p

part2 :: Int -> Int
part2 n = findgt n 1

process :: [String] -> [String]
process (row:_) = map show [part1 num, part2 num]
    where num = read row

main :: IO ()
main = interact (unlines . process . lines)
