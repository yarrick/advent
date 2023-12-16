import Data.List

step :: (Integer, Integer) -> Char -> (Integer, Integer)
step (x, y) c
    | c == '^' = (x,y+1)
    | c == 'v' = (x,y-1)
    | c == '<' = (x-1,y)
    | c == '>' = (x+1,y)

walk :: (Integer,Integer) -> String -> [(Integer,Integer)]
walk _ [] = []
walk spot (c:cc) = newspot : walk newspot cc
    where newspot = step spot c

places :: String -> [(Integer,Integer)]
places str = startspot : walk startspot str
    where startspot = (0,0)

houses str = length $ nub $ sort $ places str

-- part 2

taskSplit :: [a] -> ([a], [a])
taskSplit [] = ([], [])
taskSplit [x] = ([x], [])
taskSplit (x:y:xys) = (x:xs, y:ys) where (xs, ys) = taskSplit xys

robohouses str = length $ nub $ sort $ (santaplaces ++ roboplaces)
    where
        (santatasks, robotasks) = taskSplit str
        santaplaces = places santatasks
        roboplaces = places robotasks

process (row:_) = map show [houses row, robohouses row]

main :: IO ()
main = interact (unlines . process . lines)
