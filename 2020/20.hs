import Data.Char
import Data.List

-- tag, edges (order: top(0), right(1), bot(2), left(3)), complete data
type Tile = (Int, [String], [String])

parse :: [String] -> [Tile]
parse [] = []
parse rows = (num, [head tile, map last tile, last tile, map head tile], tile) : parse rest
    where chunk = takeWhile (/= "") rows
          num = read $ filter isDigit (head chunk)
          tile = tail chunk
          rest = drop (length chunk + 1) rows

-- returns tileside, matching tile, matchside, flipped
edgeMatch :: Int -> (Int, String) -> [Tile] -> [(Int,Int,Int,Bool)]
edgeMatch _ _ [] = []
edgeMatch num (side, row) ((t,rs,_):rrs)
    | num == t = edgeMatch num (side,row) rrs -- skip self
    | length sidematches > 0 = [(side, t, fst $ head sidematches, False)]
    | length revsidematches > 0 = [(side, t, fst $ head revsidematches, True)]
    | otherwise = edgeMatch num (side,row) rrs
    where matches rr = filter (\(s,line) -> line == row) rr
          sidematches = matches $ zip [0..] rs
          revsidematches = matches $ zip [0..] $ map reverse rs

type Placed = (Tile, [(Int,Int,Int,Bool)])

getTag tagged tag = head $ filter (\(t,_,_) -> t == tag) tagged

pData :: Placed -> [String]
pData ((_,_,d),_) = d

-- flip along horizontal axis
hflip :: Placed -> Placed
hflip ((tag,edges,content), matches) = ((tag, edges, reverse content), map inv matches)
    where inv (0,peer,pside,flip) = (2,peer,pside,flip)
          inv (2,peer,pside,flip) = (0,peer,pside,flip)
          inv (side,peer,pside,flip) = (side,peer,pside,not flip)

-- flip along vertical axis
vflip :: Placed -> Placed
vflip ((tag,edges,content), matches) = ((tag, edges, map reverse content), map inv matches)
    where inv (3,peer,pside,flip) = (1,peer,pside,flip)
          inv (1,peer,pside,flip) = (3,peer,pside,flip)
          inv (side,peer,pside,flip) = (side,peer,pside,not flip)

turn90R :: Placed -> Placed
turn90R ((tag,edges,content), matches) = ((tag, edges, turn content), map rot matches)
    where turn ([]:as) = []
          turn cs = (reverse $ map head cs) : turn (map tail cs)
          rot (side,p,ps,f) = (mod (succ side) 4,p,ps,f)

rotate :: Placed -> (Int,Int,Int,Bool) -> Placed
rotate tile (side,_,pside,_)
    | side == pside = turn90R $ turn90R tile
    | mod (side + 1) 4 == pside = turn90R tile
    | mod (side + 2) 4 == pside = tile
    | mod (side + 3) 4 == pside = turn90R $ turn90R $ turn90R tile

nextRight :: Placed -> [Placed] -> [Placed]
nextRight p@(tile,peers) matches
    | length right == 0 = []
    | map last (pData p) == map head (pData rotpeer) = [rotpeer]
    | otherwise = [hflip rotpeer]
    where right = filter (\(side,_,_,_) -> side == 1) peers
          m@(_,peertag,peerside,flipped) = head right
          peerplace = head $ filter (\((t,_,_),_) -> t == peertag) matches
          rotpeer@(r,_) = rotate peerplace m

nextBelow :: Placed -> [Placed] -> [Placed]
nextBelow p@(tile,peers) matches
    | length below == 0 = []
    | last (pData p) == head (pData rotpeer) = [rotpeer]
    | otherwise = [vflip rotpeer]
    where below = filter (\(side,_,_,_) -> side == 2) peers
          m@(_,peertag,pside,flipped) = head below
          peerplace = head $ filter (\((t,_,_),_) -> t == peertag) matches
          rotpeer@(r,_) = rotate peerplace m

buildRow :: Placed -> [Placed] -> [Placed]
buildRow start tiles
    | right == [] = [start]
    | otherwise = start : buildRow latest tiles
    where right = nextRight start tiles
          latest = head right

build :: Placed -> [Placed] -> [[Placed]]
build start tiles
    | below == [] = [buildRow start tiles]
    | otherwise = (buildRow start tiles) : build (head below) tiles
    where below = nextBelow start tiles

contents :: [[Placed]] -> [String]
contents puzzle = concatMap (map concat) levels
    where image = map (map pData) puzzle
          chunk ([]:as) = []
          chunk as = (map head as) : chunk (map tail as)
          levels = map chunk image

--            1111111111
-- |01234567890123456789
-- |                  #
-- |#    ##    ##    ###
-- | #  #  #  #  #  #
snake :: [String] -> Bool
snake rows
    | length rows < 3 = False
    | length r1 < 20 = False
    | r1 !! 18 /= '#' = False
    | not $ all (\c -> r2 !! c == '#') [0, 5, 6, 11, 12, 17, 18, 19] = False
    | otherwise = all (\c -> r3 !! c == '#') [1, 4, 7, 10, 13, 16]
    where r1 = rows !! 0
          r2 = rows !! 1
          r3 = rows !! 2

snakes :: Int -> [String] -> [(Int,Int)]
snakes _ ([]:as) = []
snakes c photo = res ++ snakes (succ c) (map (drop 1) photo)
    where rows = map (\r -> (r, drop r photo)) [0..length photo]
          res = map (\(r,_) -> (r,c)) $ filter (\(r,rs) -> snake rs) rows

trim :: Placed -> Placed
trim ((t,e,d),pp) = ((t,e,cleaned $ map cleaned d),pp)
    where cleaned r = drop 1 $ take (length r - 1) r

hashes :: (Placed, [(Int,Int)]) -> Int
hashes (pz, spos) = hs - (length spos)*15
    where hs = length $ filter (=='#') $ concat $ pData pz

process tiles = map show [product $ map (\((t,_,_),_) -> t) $ corners, hashes found]
    where matched = map (\tile@(t,edges,_) -> (tile, concatMap (\r -> edgeMatch t r tiles) $ zip [0..] edges)) tiles
          corners = filter (\(_,ms) -> length ms == 2) matched
          start = head $ filter (\(t,ms) -> all (\(t,_,_,flip) -> (t >= 1 && t <= 2) && not flip) ms) corners
          tiledata ((_,_,td),_) = td
          solved = ((0,[], contents $ map (map trim) $ build start matched), [])
          turns = take 4 $ iterate turn90R solved
          flips = turns ++ map hflip turns
          found = head $ filter (\(m,s) -> not $ null s) $ map (\m -> (m, snakes 0 $ pData m)) flips

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
