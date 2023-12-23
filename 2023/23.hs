import Data.List
import Data.Matrix

data Dir = North | South | West | East deriving (Eq, Ord, Show)
type Step = ((Int,Int),Dir)
type Route = ((Int,Int),(Int,Int),Int)

nodir d = map fst d

process :: [String] -> [String]
process rows = map (show.maximum.follow (last dots)) [[(0,[start],map route oneway)], [(0,[start],map route allpaths)]]
    where m = fromLists rows
          rcells (r,row) = [((r,c),s) | (c,s) <- zip [1..] row ]
          dots  = map fst $ filter (\(p,c) -> c == '.') $ concatMap rcells $ zip [1..] rows
          start = head dots
          route r = (fst $ head r, fst $ last r, length r - 1)
          doubled ds = concat [ [s, reverse $ map (\(p,d) -> (p,opposite d)) s] | s <- ds ]
          sameends a b = map fst (sort [head a, last a]) == map fst (sort [head b, last b])
          allpaths = doubled $ nubBy sameends $ search m [] [[(start,South)]]
          oneway = filter (all (passable m)) allpaths

follow :: (Int,Int) -> [(Int,[(Int,Int)],[Route])] -> [Int]
follow _ [] = []
follow tgt ((len,(pos:p),routes):ws)
    | pos == tgt = len : follow tgt ws
    | otherwise = follow tgt $ reverse $ sort $ cands ++ ws
    where next n@(f,t,l) = (len+l,t:f:p,filter (n/=) routes)
          cands = map next $ filter (\(f,t,_) -> f == pos && notElem t p) routes

passable :: Matrix Char -> Step -> Bool
passable m ((r,c),dir)
    | cc == '.' = True
    | dir == North && cc == '^' = True
    | dir == South && cc == 'v' = True
    | dir == West && cc == '<' = True
    | dir == East && cc == '>' = True
    | otherwise = False
    where cc = m ! (r,c)

opposite :: Dir -> Dir
opposite North = South
opposite South = North
opposite West = East
opposite East = West

search :: Matrix Char -> [(Int,Int)] -> [[Step]] -> [[Step]]
search m f [] = []
search m found (p:paths) = walked : search m (found ++ nodir walked) (paths++todo)
    where (walked,cands) = walk m (last p) p
          todo = filter unseen $ filter (\c -> notElem (nodir c) (map nodir paths)) cands
          unseen c = notElem (fst $ last c) found

walk :: Matrix Char -> Step -> [Step] -> ([Step], [[Step]])
walk m ((r,c),dir) prev
    | length nexts == 1 = walk m (head nexts) (prev++[head nexts])
    | otherwise = (prev, map (\p -> [((r,c),dir),p]) nexts)
    where around = [ ((r-1,c),North), ((r,c-1),West), ((r,c+1),East), ((r+1,c),South) ]
          inner = filter (\((y,x),_) -> y > 0 && y <= nrows m && x > 0 && x <= ncols m) around
          nexts = filter (\(p,_) -> notElem p (nodir prev) && m ! p /= '#') inner

main :: IO ()
main = interact (unlines . process . lines)
