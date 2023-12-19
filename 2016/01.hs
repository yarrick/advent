data Direction = Lefto | Righto deriving Show

distance :: String -> Int
distance str = (abs x) + (abs y)
  where (d,x,y) = foldl walk (0,0,0) $ map decode $ words str

decode :: String -> (Direction, Int)
decode (a:bb) = (dir a, read $ takeWhile (/= ',') bb)
  where  dir 'L' = Lefto
         dir 'R' = Righto

walk :: (Int,Int,Int) -> (Direction,Int) -> (Int,Int,Int)
walk (dir,x,y) (move,len) = (newdir,nx,ny)
  where newdir = turn dir move
        (nx,ny) = step newdir (x,y) len

turn :: Int -> Direction -> Int
turn x Lefto = x-1
turn x Righto = x+1

step :: Int -> (Int,Int) -> Int-> (Int,Int)
step dir (x,y) len = (x + (mx*len),y + (my*len))
  where (mx,my) = moves dir

moves :: Int -> (Int,Int)
moves dir
 | d == 0 = (1,0)
 | d == 1 = (0,1)
 | d == 2 = (-1,0)
 | d == 3 = (0,-1)
   where d = dir `mod` 4

-- part 2

distance2 str = (abs x) + (abs y)
 where (d,x,y,h) = walk2 (0,0,0,[(0,0)]) $ map decode $ words str

walk2 :: (Int,Int,Int,[(Int,Int)]) -> [(Direction,Int)] -> (Int,Int,Int,[(Int,Int)])
walk2 a [] = a
walk2 (dir,x,y,hist) ((move,len):mm)
 | collision == [] = walk2 (newdir,nx,ny,pos ++ hist) mm
 | otherwise = (newdir, cx, cy, hist)
  where newdir = turn dir move
        pos = map (step newdir (x,y)) (take len [1..])
        (nx,ny) = last pos
        collision = hits $ collide hist pos
        (cx, cy, ct) = head collision

collide :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int,Bool)]
collide _ [] = []
collide hist ((x,y):pos) = (x,y,elem (x,y) hist) : collide hist pos

hits :: [(Int,Int,Bool)] -> [(Int,Int,Bool)]
hits pos = take 1 $ dropWhile miss pos

miss :: (Int,Int,Bool) -> Bool
miss (a,b,c) = not c

process :: [String] -> [String]
process (row:_) = map show [distance row, distance2 row]

main :: IO ()
main = interact (unlines . process . lines)
