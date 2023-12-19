import Data.Maybe

code str = c
  where (x,y,c) = foldl walk (1,1,[]) $ words str

walk :: (Int,Int,String) -> String -> (Int,Int,String)
walk (x,y,c) [] = (x,y,c ++ [getKey x y])
walk (x,y,c) (a:bb)
 | isNothing newpos = walk (x,y,c) bb
 | otherwise = walk (nx,ny,c) bb
  where newpos = move x y (diff a)
        (nx,ny) = fromJust newpos

getKey :: Int -> Int -> Char
getKey x y = head $ show $ (y * 3) + x + 1

move :: Int -> Int -> (Int,Int) -> Maybe (Int,Int)
move x y (dx,dy)
 | nx < 0 = Nothing
 | nx > 2 = Nothing
 | ny < 0 = Nothing
 | ny > 2 = Nothing
 | otherwise = Just (nx,ny)
  where nx = x + dx
        ny = y + dy

diff :: Char -> (Int,Int)
diff 'U' = (0,-1)
diff 'D' = (0,1)
diff 'L' = (-1,0)
diff 'R' = (1,0)

-- part 2

code2 str = c
  where (x,y,c) = foldl walk2 (0,2,[]) $ words str

walk2 :: (Int,Int,String) -> String -> (Int,Int,String)
walk2 (x,y,c) [] = (x,y,c ++ [getKey2 x y])
walk2 (x,y,c) (a:bb)
 | isNothing newpos = walk2 (x,y,c) bb
 | otherwise = walk2 (nx,ny,c) bb
  where newpos = move2 x y (diff a)
        (nx,ny) = fromJust newpos

getKey2 :: Int -> Int -> Char
getKey2 2 0 = '1'
getKey2 1 1 = '2'
getKey2 2 1 = '3'
getKey2 3 1 = '4'
getKey2 0 2 = '5'
getKey2 1 2 = '6'
getKey2 2 2 = '7'
getKey2 3 2 = '8'
getKey2 4 2 = '9'
getKey2 1 3 = 'A'
getKey2 2 3 = 'B'
getKey2 3 3 = 'C'
getKey2 2 4 = 'D'


move2 :: Int -> Int -> (Int,Int) -> Maybe (Int,Int)
move2 x y (dx,dy)
 | nx < 0 = Nothing
 | nx > 4 = Nothing
 | ny < 0 = Nothing
 | ny > 4 = Nothing
 | ny == 0 && nx /= 2 = Nothing
 | ny == 1 && nx < 1 = Nothing
 | ny == 1 && nx > 3 = Nothing
 | ny == 3 && nx < 1 = Nothing
 | ny == 3 && nx > 3 = Nothing
 | ny == 4 && nx /= 2 = Nothing
 | otherwise = Just (nx,ny)
  where nx = x + dx
        ny = y + dy

process :: String -> [String]
process rows = [code rows, code2 rows]

main :: IO ()
main = interact (unlines . process)
