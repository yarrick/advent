import Data.List

type Swap = ((Integer,String),[(Integer,String)])

join :: [String] -> [(Integer,String)]
join [] = []
join (num:str:xs) = (read num,delete ',' str) : join xs

parse :: String -> Swap
parse str = (head $ join $ tail output, join inputs)
  where (inputs,output) = break ("=>"==) $ words str

ranker :: Integer -> [(Integer,String)] -> [Swap] -> [(Integer,String)]
ranker n rank swaps
  | length newrank == 0 = rank
  | otherwise = ranker (n+1) (rank ++ newrank) swaps
  where buildable = map (snd . fst) $ filter (canbuild rank) swaps
        newrank = zip (repeat n) $ filter (\b -> length (getpart rank b) == 0) buildable

canbuild :: [(Integer,String)] -> Swap -> Bool
canbuild rank ((_,out),inputs) = and $ map ((1<=) .length . (getpart rank) . snd) inputs

getpart list name = filter (\(_,n) -> name == n) list

topranked :: [(Integer,String)] -> [(Integer,String)] -> (Integer,String)
topranked elems rank = (\(a,b,c) -> (b,c)) $ head $ reverse $ sort ranked
  where ranked = map (\(m,n) -> (fst $ head $ getpart rank n, m, n)) elems

mult :: Integer -> Integer -> Integer
mult have build
  | r == 0 = q
  | otherwise = q + 1
  where (q,r) = quotRem have build

swap :: (Integer,String) -> [Swap] -> [(Integer,String)]
swap (have,input) swaps = map (\(x,e) -> (x*mult have build,e)) elems
  where ((build,out),elems) = head $ filter (\((_,e),_) -> e == input) swaps

merge :: [(Integer,String)] -> [(Integer,String)]
merge [] = []
merge (e:[]) = e:[]
merge ((n,a):(m,b):xs)
  | a == b = merge $ (n+m,a):xs
  | otherwise = (n,a) : merge ((m,b):xs)

trade elems swaps ranks = merge $ sortBy (\(_,a) (_,b) -> compare a b) $ elx ++ swap top swaps
  where top = topranked elems ranks
        elx = delete top elems

orify ((n,"ORE"):[]) _ = n
orify elems (swaps, ranks) = orify (trade elems swaps ranks) (swaps, ranks)

part1 :: ([Swap], [(Integer,String)]) -> Integer
part1 rules = orify [(1,"FUEL")] rules

trillion rules n = orify [(n,"FUEL")] rules <= 1000000000000

seek rules low high
    | low + 1 >= high = low
    | trillion rules mid = seek rules mid high
    | otherwise = seek rules low mid
    where mid = low + ((high - low) `div` 2)

part2 :: ([Swap], [(Integer,String)]) -> Integer
part2 rules = seek rules starter (10*starter)
    where starter = last $ takeWhile (trillion rules) $ iterate (10*) 1

process :: [Swap] -> [String]
process swaps = map show [part1 rules, part2 rules]
  where rules = (swaps, ranker 1 [(0,"ORE")] swaps)

main :: IO ()
main = interact (unlines . process . map parse . lines)
