
parse :: String -> (String,String)
parse w = (base, tail orb)
    where (base, orb) = break (')'==) w

data Orbit = Planet String Int [Orbit] deriving (Show)

children :: String -> [(String,String)] -> [(String,String)]
children planet orbits = filter (\a -> planet == fst a) orbits

chain :: String -> Int -> [(String,String)] -> Orbit
chain name depth all = Planet name depth [chain o (depth + 1) all | (_,o) <- children name all]

orbits :: Orbit -> Int
orbits (Planet _ depth suborbits) = depth + sum (map orbits suborbits)

run mapdata = orbits root
  where root = chain "COM" 0 $ map parse $ words mapdata

-- part 2

spot :: Orbit -> String -> [String]
spot (Planet name _ suborbits) target
  | name == target = [name]
  | length suborbits == 0 = []
  | maxlen == 0 = []
  | otherwise = name : (head $ filter (\r -> length r > 0) results)
  where
    results = [spot o target | o <- suborbits]
    maxlen = maximum $ map length results

pathdiff :: [String] -> [String] -> Int
pathdiff [] [] = 0
pathdiff (x:xs) (y:ys)
  | x == y = pathdiff xs ys
  | otherwise = length (x:xs) + length (y:ys) - 2

run2 mapdata = pathdiff (spot root "YOU") (spot root "SAN")
  where root = chain "COM" 0 $ map parse $ words mapdata

process :: String -> [String]
process rows = map show [run rows, run2 rows]

main :: IO ()
main = interact (unlines . process)
