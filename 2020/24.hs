import Data.List

walk :: String -> (Int,Int) -> (Int,Int)
walk [] a = a
walk (w:ws) (x,y)
    | w == 'e' = walk ws (x+2,y)
    | w == 'w' = walk ws (x-2,y)
walk (w:ww:ws) (x,y)
    | w == 's' && ww == 'e' = walk ws (x+1,y-2)
    | w == 's' && ww == 'w' = walk ws (x-1,y-2)
    | w == 'n' && ww == 'e' = walk ws (x+1,y+2)
    | w == 'n' && ww == 'w' = walk ws (x-1,y+2)

neighbors :: [(Int,Int)] -> [(Int,Int)]
neighbors [] = []
neighbors (p:ps) = (map (\n -> walk n p) ["e","w","se","sw","ne","nw"]) ++ neighbors ps

flipper :: [(Int,Int)] -> String -> [(Int,Int)]
flipper black path
    | elem end black = filter (/=end) black
    | otherwise = end : black
    where end = walk path (0,0)

tick :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
tick black pos
    | elem pos black && nblack == 0 = []
    | elem pos black && nblack > 2 = []
    | elem pos black = [pos]
    | nblack == 2 = [pos]
    | otherwise = []
    where nbors = neighbors [pos]
          nblack = length $ filter (\p -> elem p black) nbors

time :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
time black day goal
    | day == goal = black
    | otherwise = time next (succ day) goal
    where next = concatMap (tick black) $ nub $ neighbors black

process :: [String] -> [String]
process m = [show $ length $ end, show $ length $ time end 0 100]
    where end = foldl flipper [] m

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
