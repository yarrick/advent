
parse :: String -> [Bool]
parse [] = []
parse ('.':ps) = False : parse ps
parse ('^':ps) = True : parse ps

tile :: [Bool] -> [Bool]
tile (a:b:[]) = []
tile (True:b:False:ts) = True : tile (b:False:ts)
tile (False:b:True:ts) = True : tile (b:True:ts)
tile (t:ts) = False : tile ts

row :: [Bool] -> [Bool]
row tiles = tile $ False : tiles ++ [False]

rows :: [Bool] -> [[Bool]]
rows r = r : rows (row r)

safe :: [[Bool]] -> Int
safe [] = 0
safe (b:bs) = (length $ filter not b) + safe bs

run str n = safe $ take n $ rows $ parse str
