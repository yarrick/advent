import Data.List
import Data.Matrix as M

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

part1 :: [Node] -> Int
part1 input = length $ filter goodpair [ (a,b) | a <- input, b <- delete a input ]

neighbors m (xs,ys) = filter validY $ filter validX possible
    where possible = [(xs,y) | y <- [ys-1,ys+1] ] ++ [(x,ys) | x <- [xs-1,xs+1] ]
          validX (r,_) = r >= 1 && r < M.nrows m
          validY (_,c) = c >= 1 && c < M.ncols m

stepPath :: Matrix (Int,Int) -> Matrix (Int,Int) -> (Int,Int) -> Matrix (Int,Int)
stepPath disks path pos
    | length vals == 0 = path
    | otherwise = M.setElem (pathlen+1, minimum [capacity,myCapacity]) pos path
    where vals = sortBy largest $ filter better $ map (\p -> path M.! p) $ neighbors path pos
          (myCapacity,myUsage) = disks M.! pos
          (mylen,_) = path M.! pos
          better (len,size) = len < mylen && size >= myUsage
          largest (la,sa) (lb,sb)
            | la == lb = compare sb sa -- want biggest
            | otherwise = compare la lb -- want lowest
          (pathlen, capacity) = head vals

--zeroPath :: Matrix (Int,Int) -> Matrix (Int,Int) -> Matrix (Int,Int)
zeroPath disks path
    | newpath == path = path
    | otherwise = zeroPath disks newpath
    where cells = [(r,c) | r <- [1..M.nrows path], c <- [1..M.ncols path] ]
          newpath = foldl (\m pos -> stepPath disks m pos) path cells

part2 :: [Node] -> Int
part2 input = initialMoves + 5*(M.nrows path - 2) + 1
    where avail (pos,size,used) = (size,used)
          rows = succ $ maximum $ map (\((r,_),_,_) -> r) input
          cols = succ $ maximum $ map (\((_,c),_,_) -> c) input
          disks = M.fromList rows cols $ map avail input
          empty = head $ map (\((r,c),_,_) -> (r+1,c+1)) $ filter (\(_,_,usage) -> usage == 0) input
          pathStart = M.setElem (0,fst $ disks M.! empty) empty $ M.fromList rows cols $ cycle [(rows*cols, 0)]
          path = zeroPath disks pathStart
          initialMoves = fst $ path M.! ((M.nrows path) - 1, 1)

prepare rows = concat $ map (parse . words) rows

process :: [String] -> [String]
process rows = map show [part1 input, part2 input]
  where input = prepare rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
