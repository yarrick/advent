import Data.List

type Tile = [String]

divide :: Tile -> [[Tile]]
divide t = splitter splitsize t
    where splitsize
            | mod (length t) 2 == 0 = 2
            | otherwise = 3

splitter :: Int -> Tile -> [[Tile]]
splitter _ [] = []
splitter sz t = merge (map splitrow (take sz t)) : splitter sz (drop sz t)
    where splitrow [] = []
          splitrow r = (take sz r) : splitrow (drop sz r)
          merge l
              | maximum (map length l) == 0 = []
              | otherwise = map head l : merge (map tail l)

combine :: [[Tile]] -> Tile
combine [] = []
combine ts = concat $ map combinerow ts

combinerow :: [Tile] -> Tile
combinerow ts
    | length (head ts) == 0 = []
    | otherwise = concat (map head ts) : combinerow (map tail ts)

swap :: [(Tile,Tile)] -> Tile -> Tile
swap moves t
    | matches == [] = error ("No matching swap for " ++ show t)
    | otherwise = snd $ head matches
    where matches = filter (\m -> elem (fst m) (flips t)) moves

flips :: Tile -> [Tile]
flips t = base ++ map (map reverse) base
    where base = [t, transpose t, reverse t, reverse $ transpose t]

play :: [(Tile, Tile)] -> Tile -> Tile
play swaps t = combine $ map (map (swap swaps)) $ divide t

chunk :: String -> Tile
chunk s
    | rest == [] = [c]
    | otherwise = c: chunk (tail rest)
    where (c,rest) = break (=='/') s

parse :: String -> (Tile, Tile)
parse r = (chunk from, chunk to)
    where (from:_:to:[]) = words r

process :: [String] -> [String]
process rows = map show [score 5, score 18]
    where result = iterate (play (map parse rows)) [".#.", "..#", "###"]
          score n = length $ filter (=='#') $ concat (result !! n)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
