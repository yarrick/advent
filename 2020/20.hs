import Data.Char

-- line order: top, right, bot, left
type Tile = (Int, [String])

parse :: [String] -> [Tile]
parse [] = []
parse rows = (num, [head tile, map last tile, last tile, map head tile]) : parse rest
    where chunk = takeWhile (/= "") rows
          num = read $ filter isDigit (head chunk)
          tile = tail chunk
          rest = drop (length chunk + 1) rows

-- returns tileside, matching tile, matchside, flipped
edgeMatch :: Int -> (Int, String) -> [Tile] -> [(Int,Int,Int,Bool)]
edgeMatch _ _ [] = []
edgeMatch num (side, row) ((t,rs):rrs)
    | num == t = edgeMatch num (side,row) rrs -- skip self
    | length sidematches > 0 = [(side, t, fst $ head sidematches, False)]
    | length revsidematches > 0 = [(side, t, fst $ head revsidematches, True)]
    | otherwise = edgeMatch num (side,row) rrs
    where matches rr = filter (\(s,line) -> line == row) rr
          sidematches = matches $ zip [0..] rs
          revsidematches = matches $ zip [0..] $ map reverse rs

process tiles = [show $ product $ map (\(t,_) -> t) $ filter (\(_,ms) -> length ms == 2) matched]
    where matched = map (\(t,edges) -> (t, concatMap (\r -> edgeMatch t r tiles) $ zip [0..] edges)) tiles

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
