import Data.List

powerlevel :: Int -> (Int,Int) -> Int
powerlevel serial (x,y) = mod (div pwr 100) 10 - 5
  where rackid = x+10
        pwr = (rackid * y + serial) * rackid

row :: Int -> Int -> [Int]
row serial idx = map (\x -> powerlevel serial (x,idx)) [1..300]

newgrid :: Int -> [[Int]]
newgrid serial = map (row serial) [1..300]

scorecells :: [[Int]] -> Int -> Int -> Int -> [(Int,Int)]
scorecells grid size maxsize prevsum
  | length grid < size = []
  | length (grid!!0) < size = []
  | size > maxsize = []
  | otherwise = (newsum,size) : scorecells grid (size+1) maxsize newsum
  where newsum = prevsum +  sum (map (\r -> r!!(size-1)) (take (size-1) grid)) + sum (take size (grid!!(size-1)))

scorerow grid pos maxsize
  | length (grid!!0) == 0 = []
  | otherwise = topscore ++ scorerow (map (drop 1) grid) (pos+1) maxsize
  where topscore = map (\(val,size) -> (val,(pos,size))) $ take 1 $ reverse $ sort $ scorecells grid 1 maxsize 0

scorerows [] _ _ = []
scorerows grid pos maxsize = topscore ++ scorerows (drop 1 grid) (pos+1) maxsize
  where topscore = map (\(val,(x,size)) -> (val,(x,pos,size))) $ take 1 $ reverse $ sort $ scorerow grid 1 maxsize

run serial = (\(v,(x,y,s)) -> (x,y)) $ last $ sort $ scorerows (newgrid serial) 1 3

-- Allowing bigger than 25x25 only makes it slower.
-- Too many negative numbers for bigger squares to be better.
run2 serial = last $ sort $ scorerows (newgrid serial) 1 25
