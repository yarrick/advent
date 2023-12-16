import Data.Matrix hiding (trace)
import Data.List
import Data.Char
import System.IO
import Debug.Trace

generate :: [String] -> Matrix ([Char],[Char],Int)
generate input = matrix (length input) (length $ head input) (fill input)

fill :: [String] -> (Int,Int) -> ([Char],[Char],Int)
fill input (i,j)
  | c == '#' = ([],[],-1)
  | c >= 'A' && c <= 'Z' = ([c],[],999999)
  | c >= 'a' && c <= 'z' = ([],[toUpper c],999999)
  | otherwise = ([],[],999999)
  where c = (input !! (i-1)) !! (j - 1)

locate :: [String] -> (Char -> Bool) -> [(Char,(Int,Int))]
locate input fn = map (\(x,(y,c)) -> (c,(x,y))) $ filter (\(_,(_,c)) -> fn c) $
  concat $ map (\(a,bs) -> zip (repeat a) bs) $ zip [0..] $ map (\row -> zip [0..] row) input

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 0 = fallback
  | y < 0 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem (x+1) (y+1) m

mark :: Matrix a -> (Int,Int) -> a -> Matrix a
mark m (x,y) val = setElem val (x+1,y+1) m

update :: Matrix ([Char],[Char],Int) -> (Int,Int) -> (Matrix ([Char],[Char],Int),[(Int,Int)])
update m (x,y)
  | self == -1 = (m,[])
  | self == 0 = (m,map fst neighbors)
  | length bestnbr == 0 = (m,[])
  | self <= ndist = (m,[])
  | otherwise = (mark m (x,y) (nub (mydoor++doors),nub (autokeys++keys),ndist+1), followups)
  where
    (mydoor,autokeys,self) = getxy m (x,y) ([],[],-1)
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)]
    neighbors = filter (\(_,(_,_,d)) -> d >= 0) $ zip cross (map (\pos -> getxy m pos ([],[],-1)) cross)
    bestnbr = sortBy (\(_,(_,_,a)) (_,(_,_,b)) -> compare a b) neighbors
    (pos,(doors,keys,ndist)) = head bestnbr
    followups = map fst $ filter (\(_,(_,_,d)) -> d > (ndist+1)) neighbors

flow :: (Matrix ([Char],[Char],Int),[(Int,Int)]) -> Matrix ([Char],[Char],Int)
flow (m,[]) = m
flow (m,(p:ps)) = flow (mm, nub $ ps ++ pps)
  where (mm,pps) = update m p

getdist :: Matrix ([Char],[Char],Int) -> (Char,(Int,Int)) -> [(Char,(Int,Int))] -> [((Char,Char),([Char],[Char],Int))]
getdist m (start,pos) ends = map (\((from,to),(doors,autokeys,dist)) -> ((from,to),(doors, delete to autokeys,dist))) keylocs
  where distm = flow (mark m pos ([],[],0), [pos])
        keylocs = map (\(end,(ex,ey)) -> ((start,end),getxy distm (ex,ey) ([],[],-1))) ends

path :: Char -> Char -> [((Char,Char),([Char],[Char],Int))] -> ([Char],[Char],Int)
path from to dists
  | from > to = path to from dists
  | otherwise = snd $ head $ filter (\((f,t),_) -> f == from && t == to) dists

data Output = Result String Int |
              PossiblePath (Char,([Char],Int)) [Char] deriving (Eq,Show)

type Distance = ((Char,Char),([Char],[Char],Int))

mostkeys :: (Char,([Char],[Char],Int)) -> (Char,([Char],[Char],Int)) -> Ordering
mostkeys (_,(da,ka,sa)) (_,(db,kb,sb)) = compare kb ka

expand :: (Char,([Char],Int)) -> [Distance] -> [Char] -> [Output]
expand (_,(carry,steps)) _ [] = [Result carry steps]
expand (c,(carry,steps)) dists keys =
  map (\(k,(drs,ks,dist)) -> PossiblePath (k,(nub $ carry ++ ks ++ [c], steps+dist)) (delete k keys)) viable
  where
    openable (_,(d,ak,_)) = length d == length (intersect d (nub $ c:carry++ak))
    viable = filter openable $ map (\k -> (k,path c k dists)) keys

shortest :: Int -> Output -> Int
shortest cur (Result _ s) = minimum [s,cur]
shortest cur _ = cur

workpath :: [Distance] -> Int -> [Output] -> [Output]
workpath _ _ [] = []
workpath dists stoplen ((Result p steps):os)
  | steps >= stoplen = os
  | otherwise = Result p steps : workpath dists stoplen os
workpath dists stoplen ((PossiblePath pos@(_,(_,steps)) keys):os)
  | steps >= stoplen = os
  | length exp == 0 = workpath dists stoplen os
  | otherwise = exp ++ os
  where exp = expand pos dists keys

steppath :: [Distance] -> ([Output],Int) -> ([Output],Int)
steppath dists (os,stoplen) = (workpath dists stoplen os, foldl shortest stoplen os)

compress :: [Int] -> [Int]
compress (a:[]) = [a]
compress (a:b:cs)
  | b < a = b : compress (b:cs)
  | otherwise = compress (a:cs)

complete :: [Distance] -> ([Output],Int) -> [([Output],Int)]
complete dists os
  | os == next = [os]
  | otherwise = os : complete dists next
  where next = steppath dists os

twiddle :: [Distance] -> Int -> Char -> [Char]
twiddle dists numkeys key
  | length keypaths > 0 && length locked == 0 = []
  | otherwise = [key]
  where keypaths = filter (\((from,to),(_,autokeys,_)) -> from == '@' && elem key autokeys) dists
        locked = filter (\(_,(doors,_,_)) -> elem key doors) dists

cleandist :: [Distance] -> [Char]
cleandist dists = concatMap (twiddle dists (length keys)) keys
  where keys = nub $ map (snd.fst) dists

filterdistkey :: [Char] -> Distance -> Distance
filterdistkey keys (path,(doors,autokeys,dist)) = (path,(fk doors,fk autokeys,dist))
  where fk str = filter (\s -> elem s keys) str

grouptree :: [([Char],[(Char,[Char])])] -> Distance -> [([Char],[(Char,[Char])])]
grouptree [] ((_,key),(doors,autokeys,_)) = [(doors,[(key,autokeys)])]
grouptree ((d,keys):ds) dist@((_,key),(doors,autokeys,_))
  | d == doors = (d,(key,autokeys):keys) : ds
  | otherwise = (d,keys) : grouptree ds dist

cleangroup :: ([Char],[(Char,[Char])]) -> ([Char],[(Char,[Char])])
cleangroup (doors,keys) = (doors, sortBy ranker $ map (\(k,autoks) -> (k, intersect groupkeys autoks)) keys)
  where groupkeys = map fst keys
        ranker (_,a) (_,b) = compare (length b) (length a)

keysrequired :: String -> (Int,String) -> Bool
keysrequired keys doorset = or $ map (\k -> elem k (snd doorset)) keys

filterkey :: [(Int,String)] -> (Char,String) -> [Char]
filterkey doorsets (key,autokeys)
  | intersect reqauto reqkey == reqauto = autokeys
  | otherwise = []
  where reqauto = filter (keysrequired autokeys) doorsets
        reqkey = filter (keysrequired [key]) doorsets

scrapkeys :: [(Int,String)] -> [(String,[(Char,String)])] -> [Char]
scrapkeys _ [] = []
scrapkeys doorsets ((doors,keys):ds) = (concat $ map (filterkey doorsets) keys) ++ scrapkeys doorsets ds

redundantkeys :: [Distance] -> [Char] -> [Char]
redundantkeys dists keys = nub $ scrapkeys doorsets grouped
  where
    rootdists = map (\(a,(ds,ak,dists)) -> (a,(intersect keys ds,intersect keys ak,dists))) $
      filter (\((from,to),_) -> from == '@' && elem to keys) dists
    grouped = map cleangroup $ foldl grouptree [] rootdists
    doorsets = zip [0..] $ map fst grouped

part1 :: [String] -> Matrix ([Char],[Char],Int) -> [(Char,(Int,Int))] -> [(Char,(Int,Int))] -> Int
part1 rows m starters keys
  | length starters == 1 = last $ (compress $ map snd $ complete filtdists trail)
  | otherwise = -1
  where (starter,startpos) = head starters
        dists = getdist m (starter,startpos) keys ++ concat [ getdist m k (dropWhile (k>=) keys) | k <- keys ]
        filteredkeys = cleandist dists
        goodkeys = map fst $ filter (\(c,_) -> elem c filteredkeys) keys
        skipkeys = redundantkeys dists goodkeys
        betterkeys = filter (\c -> not $ elem c skipkeys) goodkeys
        filtdists = map (filterdistkey betterkeys) dists
        trail = ([PossiblePath (starter,([],0)) betterkeys],99999)

process :: [String] -> [String]
process rows = [show $ part1 rows m starters keys] ++ part2 rows m starters keys
  where m = generate rows
        starters = locate rows ('@'==)
        keys = sort $ map (\(c,p) -> (toUpper c, p)) $ locate rows (\c -> c >= 'a' && c <= 'z')

-- part 2

splitmap :: Matrix ([Char],[Char],Int) -> [(Char,(Int,Int))] -> Matrix ([Char],[Char],Int)
splitmap m ((c,(x,y)):[]) = foldl wallit m walls
  where walls = [(x-1,y),(x,y-1),(x,y),(x,y+1),(x+1,y)]
        wallit mx pos = mark mx pos ([],[],-1)

fixstarts :: [(Char,(Int,Int))] -> [(Char,(Int,Int))]
fixstarts ((c,(x,y)):[]) = zip (repeat c) [(x+1,y-1),(x+1,y+1),(x-1,y-1),(x-1,y+1)]

calcpart :: Matrix ([Char],[Char],Int) -> [(Char,(Int,Int))] -> (Char,(Int,Int)) -> (Char,[Char],[Distance])
calcpart m keys (starter,startpos) = (starter,goodkeys,dists)
  where startdist = filter (\(_,(_,_,dist)) -> dist < 999999) $ getdist m (starter,startpos) keys
        foundkeys = sort $ map (snd.fst) $ startdist
        mykeys = filter (\(c,_) -> elem c foundkeys) keys
        dists = startdist ++ concat [ getdist m k (dropWhile (k>=) mykeys) | k <- mykeys ]
        filteredkeys = cleandist dists
        goodkeys = map fst $ filter (\(c,_) -> elem c filteredkeys) mykeys

-- solved by hand based on this print
part2 :: [String] -> Matrix ([Char],[Char],Int) -> [(Char,(Int,Int))] -> [(Char,(Int,Int))] -> [String]
part2 rows m starters keys
  | length starters == 1 = part2 rows (splitmap m starters) (fixstarts starters) keys
  | otherwise = map show parts
  where rawparts = map (calcpart m keys) starters
        allkeys = sort $ concat $ map (\(_,ks,_) -> ks) parts
        parts = map (\(st,ks,dists) -> (st,ks, map (filterdistkey allkeys) dists)) rawparts

main :: IO ()
main = interact (unlines . process . lines)
