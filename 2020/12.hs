data Dir = North | East | South | West deriving (Eq, Show, Enum)

parse :: String -> (Char,Int)
parse (a:bs) = (a, read bs)

type Pos = (Int,Int,Dir)

step :: Pos -> (Char,Int) -> Pos
step (ns,ew,dir) ('N',len) = (ns+len,ew,dir)
step (ns,ew,dir) ('E',len) = (ns,ew+len,dir)
step (ns,ew,dir) ('S',len) = (ns-len,ew,dir)
step (ns,ew,dir) ('W',len) = (ns,ew-len,dir)
step (ns,ew,dir) ('F',len)
    | dir == North = (ns+len,ew,dir)
    | dir == East = (ns,ew+len,dir)
    | dir == South = (ns-len,ew,dir)
    | dir == West = (ns,ew-len,dir)
step p ('R',0) = p
step (ns,ew,West) ('R',turns) = step (ns,ew,North) ('R',turns-90)
step (ns,ew,dir) ('R',turns) = step (ns,ew,succ dir) ('R',turns-90)
step p ('L',0) = p
step (ns,ew,North) ('L',turns) = step (ns,ew,West) ('L',turns-90)
step (ns,ew,dir) ('L',turns) = step (ns,ew,pred dir) ('L',turns-90)

-- Direction in Pos unused here, but easier to share code with previous part.
step2 :: (Pos,Pos) -> (Char,Int) -> (Pos,Pos)
step2 ((sns,sew,sd),(wns,wew,wd)) ('F',len) = ((sns+len*wns,sew+len*wew,sd),(wns,wew,wd))
step2 p ('R',0) = p
step2 (ship,(wns,wew,wd)) ('R',turn) = step2 (ship, (-wew,wns,wd)) ('R',turn-90)
step2 p ('L',0) = p
step2 (ship,(wns,wew,wd)) ('L',turn) = step2 (ship, (wew,-wns,wd)) ('L',turn-90)
step2 (ship,wp) (d,len)
    | elem d "NESW" = (ship, step wp (d,len))

dist :: Pos -> Int
dist (ns,ew,_) = abs ns + abs ew

process :: [(Char,Int)] -> [String]
process m = map (show.dist) [foldl step (0,0,East) m, fst $ foldl step2 ((0,0,East),(1,10,East)) m]

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
