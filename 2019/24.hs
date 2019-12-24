import Data.Matrix

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x > nrows m = fallback
  | y > ncols m = fallback
  | otherwise = getElem x y m

bugstep :: Matrix Char -> (Int,Int) -> Char
bugstep m (x,y)
  | self == '#' && length neighbors == 1 = '#'
  | self == '#'  = '.'
  | self == '.' && length neighbors > 0 && length neighbors < 3 = '#'
  | otherwise  = '.'
  where
    self = getxy m (x,y) '.'
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = filter ('#'==) $ map (\pos -> getxy m pos '.') cross

nextmap :: Matrix Char -> Matrix Char
nextmap m = fromList 5 5 $ map (bugstep m) $ [ (x,y) | x <- [1..(nrows m)], y <- [1..(ncols m)]]

finddup :: [Matrix Char] -> Matrix Char -> Matrix Char
finddup log m
  | elem mm log = mm
  | otherwise = finddup (mm:log) mm
  where mm = nextmap m

score :: Matrix Char -> Integer
score m = sum $ map (2^) bugs
  where bugs = map fst $ filter (\(_,b) -> b == '#') $ zip [0..] $ toList m

run firststate = score $ finddup [] m
   where m = fromLists firststate

