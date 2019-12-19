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
              PossiblePath (Char,(Int,Int),([Char],Int)) [(Char,(Int,Int))] deriving (Eq,Show)

type Distance = ((Char,Char),([Char],[Char],Int))

mostkeys :: (Char,(Int,Int),([Char],[Char],Int)) -> (Char,(Int,Int),([Char],[Char],Int)) -> Ordering
mostkeys (_,_,(da,ka,sa)) (_,_,(db,kb,sb)) = compare kb ka

expand :: (Char,(Int,Int),([Char],Int)) -> [Distance] -> [(Char,(Int,Int))] -> [Output]
expand (_,_,(carry,steps)) _ [] = [Result carry steps]
expand (c,startpos,(carry,steps)) dists keys =
  map (\(k,kp,(drs,ks,dist)) -> PossiblePath (k,kp,(nub $ carry ++ ks ++ [c], steps+dist)) (delete (k,kp) keys)) viable
  where
    openable (_,_,(d,ak,_)) = length d == length (intersect d (nub $ c:carry++ak))
    viable = filter openable $ map (\(k,kp) -> (k,kp,path c k dists)) keys

shortest :: Int -> Output -> Int
shortest cur (Result _ s) = minimum [s,cur]
shortest cur _ = cur

workpath :: [Distance] -> Int -> [Output] -> [Output]
workpath _ _ [] = []
workpath dists stoplen ((Result p steps):os)
  | steps >= stoplen = os
  | otherwise = Result p steps : workpath dists stoplen os
workpath dists stoplen ((PossiblePath pos@(_,_,(_,steps)) keys):os)
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
scrapkeys doorsets ((doors,keys):ds) = nub (concat $ map (filterkey doorsets) keys) ++ scrapkeys doorsets ds

redundantkeys :: [Distance] -> [(Char,(Int,Int))] -> [Char]
redundantkeys dists keypos = scrapkeys doorsets grouped
  where
    keys = map fst keypos
    rootdists = map (\(a,(ds,ak,dists)) -> (a,(intersect keys ds,intersect keys ak,dists))) $
      filter (\((from,to),_) -> from == '@' && elem to keys) dists
    grouped = map cleangroup $ foldl grouptree [] rootdists
    doorsets = zip [0..] $ map fst grouped

process :: [String] -> [String]
process rows = [show $ last $ (compress $ map snd $ complete filtdists trail)]
  where m = generate rows
        (starter,startpos) = head $ locate rows ('@'==)
        keys = sort $ map (\(c,p) -> (toUpper c, p)) $ locate rows (\c -> c >= 'a' && c <= 'z')
        dists = getdist m (starter,startpos) keys ++ concat [ getdist m k (dropWhile (k>=) keys) | k <- keys ]
        filteredkeys = cleandist dists
        goodkeys = filter (\(c,_) -> elem c filteredkeys) keys
        skipkeys = redundantkeys dists goodkeys
        betterkeys = filter (\(c,_) -> not $ elem c skipkeys) goodkeys
        filtdists = map (filterdistkey $ map fst betterkeys) dists
        trail = ([PossiblePath (starter,startpos,([],0)) betterkeys],99999)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)