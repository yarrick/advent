import Data.List

parse :: (Int,String) -> [(Int,Int)]
parse (y,str) = zip xs $ repeat y
  where xs = map fst $ filter (\x -> snd x == '#') $ zip [0..] str

asteroids :: String -> [(Int,Int)]
asteroids str = concat $ map parse $ zip [0..] (words str)

dist :: (Int,Int) -> (Int,Int) -> (Int,Int)
dist (x,y) (a,b) = (x-a,y-b)

directions :: ((Int,Int),[(Int,Int)]) -> [(Int,Int)]
directions (_,[]) = []
directions (x,(b:bs))
  | x == b = directions (x,bs)
  | otherwise = (divide $ dist x b) : directions (x,bs)
  where divide (c,d) = (div c (gcd c d), div d (gcd c d))

score :: ((Int,Int),[(Int,Int)]) -> (Int,(Int,Int))
score (pos, as) = (length $ nub $ directions (pos, as), pos)

best :: [((Int,Int),[(Int,Int)])] -> (Int,(Int,Int))
best list = last $ sort $ map score list

run str = best $ zip astr (repeat astr)
  where astr = asteroids str

-- part 2

angle :: ((Int,Int),[(Int,Int)]) -> [((Int,Int),(Int,Int))]
angle (_,[]) = []
angle (x,(b:bs))
  | x == b = angle (x,bs)
  | otherwise = (dist x b, b) : angle (x,bs)

range :: ((Int,Int),(Int,Int)) -> ((Int,Int),Int,(Int,Int))
range ((a,b),pos) = ((div a g,div b g),g,pos)
  where g = gcd a b

hidden :: [((Int,Int),Int,(Int,Int))] -> [((Int,Int),[(Int,Int)])]
hidden [] = []
hidden ((a,_,p):hs) = (a, p : (map third $ takeWhile (angled a) hs)) : (hidden $ dropWhile (angled a) hs)
  where
    angled a (x,_,_) = a == x
    third (a,b,c) = c

math ((x,y),b)
  | rad <= 0 = (-rad, b)
  | otherwise = (pi + pi - rad, b)
  where rad =atan2 (realToFrac x) (realToFrac y)

shooter :: [[(Int,Int)]] -> [(Int,Int)]
shooter list
  | length (concat list) == 0 = []
  | otherwise = concat (map fst parts) ++ shooter (map snd parts)
  where parts = map (splitAt 1) list

run2 str = (\(x,y) -> x*100 + y) $ (shooter shootlist) !! 199
  where
    astr = asteroids str
    (_,pos) = best $ zip astr (repeat astr)
    shootlist = map snd $ sort $ map math $ hidden $ sort $ map range $ angle (pos, astr)
