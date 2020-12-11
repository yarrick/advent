import Data.Matrix

data Spot = Floor | Empty | Taken deriving (Eq, Show)

parse :: String -> [Spot]
parse [] = []
parse ('.':xs) = Floor : parse xs
parse ('L':xs) = Empty : parse xs
parse ('#':xs) = Taken : parse xs

adjacent :: Matrix Spot -> (Int,Int) -> [Spot]
adjacent m (rs,cs) = map get $ filter notSelf $ filter validRow $ filter validCol possible
    where possible = [(r,c) | r <- [rs-1..rs+1], c <- [cs-1..cs+1] ]
          validRow (r,_) = r >= 1 && r <= nrows m
          validCol (_,c) = c >= 1 && c <= ncols m
          notSelf (r,c) = (r,c) /= (rs,cs)
          get p = m ! p

line :: Matrix Spot -> (Int,Int) -> (Int,Int) -> [Spot]
line m (or,oc) (dr,dc)
    | r < 1 || r > nrows m = []
    | c < 1 || c > ncols m = []
    | val == Floor = line m (r,c) (dr,dc)
    | otherwise = [val]
    where (r,c) = (or+dr,oc+dc)
          val = m ! (r,c)

seatline :: Matrix Spot -> (Int,Int) -> [Spot]
seatline m pos = concatMap (line m pos) directions
    where directions = filter (/= (0,0)) [(dr,dc) | dr <- [-1..1], dc <- [-1..1] ]

seats :: Matrix Spot -> [(Int,Int)]
seats m = [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]

flow :: Matrix Spot -> Int -> (Matrix Spot -> (Int,Int) -> [Spot]) -> (Int,Int) -> Spot
flow m lim fn pos
    | val == Empty && occ == 0 = Taken
    | val == Taken && occ >= lim = Empty
    | otherwise = val
    where val = m ! pos
          occ = length $ filter (==Taken) $ fn m pos

step :: Matrix Spot -> Int -> (Matrix Spot -> (Int,Int) -> [Spot]) -> Matrix Spot
step m lim fn = fromList (nrows m) (ncols m) $ map (flow m lim fn) (seats m)

run :: Matrix Spot -> Int -> (Matrix Spot -> (Int,Int) -> [Spot]) -> Matrix Spot
run m lim fn
    | m == mm = m
    | otherwise = run mm lim fn
    where mm = step m lim fn

score :: Matrix Spot -> Int
score m = length $ filter (==Taken) $ toList m

process :: Matrix Spot -> [String]
process m = map (show.score) [run m 4 adjacent, run m 5 seatline]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . fromLists . (map parse) . lines)
