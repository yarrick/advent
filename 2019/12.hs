import Data.List

parse :: String -> [Int]
parse str = map read $ map (\p -> take ((length p) - 1) p) splits
 where splits = map (drop 2) $ words $ tail str

type Moon = (Int,[(Int,Int)])

moon :: (Int, [Int]) -> Moon
moon (idx,pos) = (idx, zip pos (repeat 0))

gravmatch :: ((Int,Int),(Int,Int)) -> Int
gravmatch ((pa,_),(pb,_))
  | pa > pb = -1
  | pa == pb = 0
  | otherwise = 1

gravadd :: (Moon, Moon) -> [(Int,[Int])]
gravadd ((a,aa),(b,bb)) = (a, gg aa bb) : (b, gg bb aa) : []
  where gg a b = map gravmatch $ zip a b

applygrav :: [Moon] -> [(Int,[Int])] -> [Moon]
applygrav [] _ = []
applygrav m [] = m
applygrav (m@(a,aa):as) diffs@((b,bb):bs)
  | a < b = m : applygrav as diffs
  | a == b = applygrav ((a, map (\((p,v),d) -> (p,v+d)) $ zip aa bb):as) bs

grav ms = applygrav ms deltav
  where
    biggermoon (a,_) (b,_) = b > a
    deltav = sort $ concat $ map gravadd $ [ (a,b) | a <- ms, b <- filter (biggermoon a) ms ]

tick :: [Moon] -> a ->  [Moon]
tick ms _ = map move $ grav ms
  where move (idx, pv) = (idx, [ (p + v, v) | (p,v) <- pv ])

energy :: Moon -> Int
energy (_,posvel) = (sum $ map abs ps) * (sum $ map abs vs)
  where (ps,vs) = unzip posvel

run str iter = sum $ map energy $ foldl tick moons $ take iter [0..]
  where moons = map moon $ zip [0..] $ map parse str
