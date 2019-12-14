import Data.List

check :: [Int] -> Bool
check nums = (last all) < (sum $ take 2 all)
  where all = sort nums

-- (x,y), size, used
type Node  = ((Int,Int),Int,Int)

parse :: [String] -> [Node]
parse (_:"df":xs) = []
parse (host:size:used:avail:pct:[])
  | take 4 host == "File" = []
  | otherwise = [((read x, read $ drop 2 y),bytes size,bytes used)]
  where (x,y) = break ('-'==) $ drop 16 host
        bytes str = read (delete 'T' str)

goodpair :: (Node,Node) -> Bool
goodpair ((_,as,au),(_,bs,bu))
  | au == 0 = False
  | au + bu < bs = True
  | otherwise = False

part1 :: [Node] -> String
part1 input = show $ length $ filter goodpair [ (a,b) | a <- input, b <- delete a input ]

part2 :: [Node] -> String
part2 _ = []

prepare rows = concat $ map (parse . words) rows

process :: [String] -> [String]
process rows = [part1 input, part2 input]
  where input = prepare rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
