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

