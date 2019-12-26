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

-- part 2

get2xy :: [(Int,Matrix a)] -> (Int,(Int,Int)) -> a -> a
get2xy m (lvl,(x,y)) fallback
  | x < 1 = fallback
  | y < 1 = fallback
  | x > nrows (snd $ head m) = fallback
  | y > ncols (snd $ head m) = fallback
  | otherwise = getElem x y $ snd $ head $ (filter (\(i,_) -> i == lvl)) m

adjacent :: (Int,(Int,Int)) -> [(Int,(Int,Int))]
adjacent (level,(x,y))
  | x == 3 && y == 2 = standard ++ zip (repeat (level+1)) [(1,1), (2,1), (3,1), (4,1), (5,1)]
  | x == 3 && y == 4 = standard ++ zip (repeat (level+1)) [(1,5), (2,5), (3,5), (4,5), (5,5)]
  | x == 2 && y == 3 = standard ++ zip (repeat (level+1)) [(1,1), (1,2), (1,3), (1,4), (1,5)]
  | x == 4 && y == 3 = standard ++ zip (repeat (level+1)) [(5,1), (5,2), (5,3), (5,4), (5,5)]
  | x == 1 && y == 1 = standard ++ [(level-1,(2,3)),(level-1,(3,2))]
  | x == 1 && y == 5 = standard ++ [(level-1,(2,3)),(level-1,(3,4))]
  | x == 1 = standard ++ [(level-1,(2,3))]
  | x == 5 && y == 1 = standard ++ [(level-1,(4,3)),(level-1,(3,2))]
  | x == 5 && y == 5 = standard ++ [(level-1,(4,3)),(level-1,(3,4))]
  | x == 5 = standard ++ [(level-1,(4,3))]
  | y == 1 = standard ++ [(level-1,(3,2))]
  | y == 5 = standard ++ [(level-1,(3,4))]
  | otherwise = standard
  where standard = zip (repeat level) [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]

bug2step :: [(Int,Matrix Char)] -> (Int,(Int,Int)) -> Char
bug2step m pos@(_,(x,y))
  | x == 3 && y == 3 = '.'
  | self == '#' && length neighbors == 1 = '#'
  | self == '#'  = '.'
  | self == '.' && length neighbors > 0 && length neighbors < 3 = '#'
  | otherwise  = '.'
  where
    self = get2xy m pos '.'
    neighbors = filter ('#'==) $ map (\pos -> get2xy m pos '.') $ adjacent pos

stepmap :: [(Int,Matrix Char)] -> Int -> (Int,Matrix Char)
stepmap ms idx = (idx, fromList 5 5 $ map (bug2step ms) $ [ (idx,(x,y)) | x <- [1..5], y <- [1..5]])

steplevels :: [(Int,Matrix Char)] -> [(Int,Matrix Char)]
steplevels (l:ls) = l : map steplevel levels ++ [last ls]
  where levels = take (length ls - 1) ls
        steplevel (lvl,_) = stepmap (l:ls) lvl

run2 firststate = length $ filter ('#'==) $ concatMap (toList.snd) processed
  where m = fromLists firststate
        upper = take 110 $ zip [-110..] (repeat $ matrix 5 5 (\x -> '.'))
        lower = take 110 $ zip [1..] (repeat $ matrix 5 5 (\x -> '.'))
        processed = last $ take 201 $ iterate steplevels (upper ++ [(0,m)] ++ lower)
