import Data.Matrix

parse :: String -> [Bool]
parse [] = []
parse (x:xs) = (x == '#') : parse xs

type Cube = [(Int, Matrix Bool)]
type Hyper = [(Int, Cube)]

grow :: Matrix Bool -> Cube
grow m = [(0,matrix size size gen)]
    where padding = 7
          size = padding + nrows m + padding
          gen (r,c)
            | r - padding < 1 || c - padding < 1 = False
            | r - padding > nrows m || c - padding > ncols m = False
            | otherwise = m ! (r - padding,c - padding)

value :: Hyper -> (Int,Int,(Int,Int)) -> Bool
value hyper (w,z,p)
    | length cube == 0 = False
    | length slice == 0 = False
    | otherwise = (snd $ head slice) ! p
    where cube = filter (\(ww,_) -> ww == w) hyper
          slice = filter (\(zz,_) -> zz == z) $ snd $ head cube

activeAround :: Hyper -> (Int,Int,(Int,Int)) -> Int
activeAround h (ws,zs,(rs,cs)) = length $ filter id $ map (value h) candidates
    where possible = [(w,z,(r,c)) | w <- [ws-1..ws+1], z <- [zs-1..zs+1], r <- [rs-1..rs+1], c <- [cs-1..cs+1]]
          validRow (_,_,(r,_)) = r >= 1 && r <= nrows (snd $ head $ snd $ head h)
          validCol (_,_,(_,c)) = c >= 1 && c <= ncols (snd $ head $ snd $ head h)
          notSelf (w,z,(r,c)) = (w,z,(r,c)) /= (ws,zs,(rs,cs))
          candidates = filter notSelf $ filter validRow $ filter validCol possible

nextCell :: Hyper -> (Int,Int,(Int,Int)) -> Bool
nextCell h p
    | value h p && neighbors >= 2 && neighbors <= 3 = True
    | value h p = False
    | neighbors == 3 = True
    | otherwise = False
    where neighbors = activeAround h p

nextSlice :: Hyper -> Int -> Int -> (Int, Matrix Bool)
nextSlice hyper w z = (z, matrix size size (\pos -> nextCell hyper (w,z,pos)) )
    where size = nrows $ snd $ head $ snd $ head hyper

nextCube :: Hyper -> Int -> (Int, Cube)
nextCube hyper w = (w, map (nextSlice hyper w) [lo..hi])
    where lvls = concatMap (map fst.snd) hyper
          lo = minimum lvls - 1
          hi = maximum lvls + 1

nextHyperSlice :: Hyper -> Hyper
nextHyperSlice hyper = map (nextCube hyper) (map fst hyper)

nextHyper :: Hyper -> Hyper
nextHyper hyper = map (nextCube hyper) [lo..hi]
    where cubes = map fst hyper
          lo = minimum cubes - 1
          hi = maximum cubes + 1

scoreCube :: Cube -> Int
scoreCube [] = 0
scoreCube ((_,s):gs) = (length $ filter id $ toList s) + scoreCube gs

score :: Hyper -> Int
score hyper = sum $ map (scoreCube.snd) hyper

steps fn input count
    | count == 0 = input
    | otherwise = steps fn (fn input) (pred count)

process :: Matrix Bool -> [String]
process m = map (show.score) [steps nextHyperSlice hyper 6, steps nextHyper hyper 6]
   where hyper = [(0,grow m)]

main :: IO ()
main = interact (unlines . process . fromLists . (map parse) . lines)
