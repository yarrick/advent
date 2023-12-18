data Dir = North | South | West | East deriving (Eq, Ord, Show)
type Pos = (Int,Int)

process :: [((Dir, Int), (Dir, Int))] -> [String]
process rows = map (show.repair) [digs fst, digs snd]
    where digs fn = shoelace $ grow $ scanl next ((0,0),West) $ map fn rows
          grow ps = (\p -> p ++ [head p]) $ extend $ (last ps):(tail ps)
          repair n = (abs n) `div` 8 -- /4 from extend doubling, /2 from shoelace

extend :: [(Pos,Dir)] -> [Pos] -- Move point from center of box to correct corner
extend (a:[]) = []
extend (((r1,c1),d1):(p,d2):cs)
    | d1 == North && d2 == East = (r-1,c-1) : extend ((p,d2):cs)
    | d1 == North && d2 == West = (r+1,c-1) : extend ((p,d2):cs)
    | d1 == South && d2 == East = (r-1,c+1) : extend ((p,d2):cs)
    | d1 == South && d2 == West = (r+1,c+1) : extend ((p,d2):cs)
    | d1 == East && d2 == North = (r-1,c-1) : extend ((p,d2):cs)
    | d1 == East && d2 == South = (r-1,c+1) : extend ((p,d2):cs)
    | d1 == West && d2 == North = (r+1,c-1) : extend ((p,d2):cs)
    | d1 == West && d2 == South = (r+1,c+1) : extend ((p,d2):cs)
    where r = r1*2 -- doubling to avoid floats above
          c = c1*2

shoelace :: [(Int,Int)] -> Int
shoelace (a:[]) = 0
shoelace ((ra,ca):(rb,cb):cs) = (ra*cb) - (ca*rb) + shoelace ((rb,cb):cs)

next :: (Pos,Dir) -> (Dir,Int) -> (Pos,Dir)
next ((r,c),_) (North,n) = ((r-n,c),North)
next ((r,c),_) (South,n) = ((r+n,c),South)
next ((r,c),_) (West,n) = ((r,c-n),West)
next ((r,c),_) (East,n) = ((r,c+n),East)

parse :: String -> ((Dir, Int), (Dir, Int))
parse s
    | d == "R" = ((East, len), (d2,len2))
    | d == "U" = ((North, len), (d2,len2))
    | d == "L" = ((West, len), (d2,len2))
    | d == "D" = ((South, len), (d2,len2))
    where (d:n:c:_) = words s
          len = read n
          d2 = [East, South, West, North] !! (read $ [c !! 7])
          len2 = read $ "0x" ++ (take 5 $ drop 2 c)

main :: IO ()
main = interact (unlines . process . map parse . lines)
