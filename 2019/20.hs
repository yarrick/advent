import Data.Matrix
import Data.List
import System.IO

-- level, (x,y)
type Position = (Int,(Int,Int))

generate :: [String] -> Matrix ([Position],Int)
generate input = matrix (length input) (length $ head input) (fill input)

fill :: [String] -> (Int,Int) -> ([Position],Int)
fill input (i,j)
  | c == '.' = ([],999999)
  | otherwise = ([],-1)
  where c = (input !! (i-1)) !! (j - 1)

getxy :: [Matrix a] -> Position -> a -> a
getxy m (lvl,(x,y)) fallback
  | x < 0 = fallback
  | y < 0 = fallback
  | x >= nrows (m!!0) = fallback
  | y >= ncols (m!!0) = fallback
  | lvl >= length m = error "Level OOB"
  | otherwise = getElem (x+1) (y+1) (m!!lvl)

mark :: [Matrix a] -> Position -> a -> [Matrix a]
mark ms (lvl,(x,y)) val
  | lvl >= length ms = error "Level OOB"
  | otherwise = take lvl ms ++ [setElem val (x+1,y+1) (ms!!lvl)] ++ drop (lvl+1) ms

update :: [Matrix ([Position],Int)] -> Position -> Int -> ([Matrix ([Position],Int)],[Position])
update ms (lvl,(x,y)) maxval
  | self == -1 = (ms,[])
  | self == 0 = (ms,map fst neighbors)
  | self > maxval = (ms,[])
  | length bestnbr == 0 = (ms,[])
  | self <= ndist = (ms,[])
  | otherwise = (mark ms (lvl,(x,y)) (targets,ndist+1),followups)
  where
    (targets,self) = getxy ms (lvl,(x,y)) ([],-1)
    cross = zip (repeat lvl) [(x-1,y), (x,y-1), (x+1,y), (x,y+1)] ++ targets
    neighbors = filter (\(_,(_,d)) -> d >= 0) $ zip cross (map (\pos -> getxy ms pos ([],-1)) cross)
    bestnbr = sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b) neighbors
    (pos,(tg,ndist)) = head bestnbr
    followups = map fst $ filter (\(_,(_,d)) -> d > (ndist+1)) neighbors

flow :: ([Matrix ([Position],Int)],[Position],Position) -> [Matrix ([Position],Int)]
flow (ms,[],end) = ms
flow (ms,(p:ps),end) = flow (mm, nub $ ps ++ pps,end)
  where
    maxval = snd $ getxy ms end ([],-1)
    (mm,pps) = update ms p maxval

dropmargin :: [String] -> [String]
dropmargin rows = take (nrow-4) $ drop 2 $ map (take (ncol-4). drop 2) rows
  where nrow = length rows
        ncol = length (rows !! 2)

data TagSide = Inner | Outer deriving (Eq,Show)
type Tag = ((Int,Int),String,TagSide)

goodtag :: Tag -> Bool
goodtag (_,name,_) = and $ map (\c -> c >= 'A' && c <= 'Z') name

horztags :: [String] -> Int -> TagSide -> [Tag]
horztags rows x side = filter goodtag $ map (\(y,str) -> ((x,y),str,side)) pairs
  where pairs = zip [0..] $ map (\(a,b) -> a:b:[]) $ zip (rows !! 0) (rows !! 1)

verttags :: [String] -> Int -> TagSide -> [Tag]
verttags cols y side = filter goodtag $ map (\(x,str) -> ((x,y),str,side)) pairs
  where pairs = zip [0..] cols

innervtags :: [String] -> [Tag]
innervtags rows = verttags (map (take 2. drop fspace) rows) (fspace-1) Inner ++
  verttags (map (drop (lspace-1).take (lspace+1)) rows) (lspace+1) Inner
  where numrows = map (zip [0..]) rows
        spaces = map fst $ filter (\(_,c) -> c == ' ') $ concat numrows
        fspace = minimum spaces
        lspace = maximum spaces

innerhtags :: [String] -> [Tag]
innerhtags rows = horztags (map snd upper) ((fst$head upper)-1) Inner ++
  horztags (map snd lower) ((fst$last lower)+1) Inner
  where midrows = filter (\r -> elem ' ' (snd r)) $ zip [0..] rows
        upper = take 2 midrows
        lower = drop (length midrows-2) midrows

innertags :: [String] -> [Tag]
innertags rows = innervtags rows ++ innerhtags rows

tags :: [String] -> [Tag]
tags rows = horztags (map (drop 2) $ take 2 rows) 0 Outer ++
  horztags (map (drop 2) $ drop (nrow-2) rows) (nrow-5) Outer ++
  verttags (drop 2 $ map (take 2) rows) 0 Outer ++
  verttags (drop 2 $ map (drop $ ncol-2) rows) (ncol-5) Outer ++
  innertags (dropmargin rows)
  where nrow = length rows
        ncol = length (rows !! 2)

jumps :: [Tag] -> [((Int,Int),(Int,Int),TagSide)]
jumps tgs = map (\((a,_,side),(b,_,_)) -> (a,b,side)) pairs
  where
    jumptags = filter (\(_,name,_) -> name /= "AA" && name /= "ZZ") tgs
    pairs = filter (\((_,a,_),(_,b,_)) -> a == b) [ (a,b) | a <- jumptags, b <- (delete a jumptags) ]

setjump1 :: [Matrix ([Position],Int)] -> ((Int,Int),(Int,Int),TagSide) -> [Matrix ([Position],Int)]
setjump1 m (from,to,side)= mark m (0,from) ([(0,to)],999999)

process :: [String] -> [String]
process rows = [show $ snd $ getxy (flow (jump1,[startpos],endpos)) endpos ([],-1),
                show $ snd $ getxy (flow (jump2,[startpos],endpos)) endpos ([],-1)]
  where m = generate $ dropmargin rows
        taggs = tags rows
        startpos =(0, (\(p,_,_) -> p) $ head $ filter (\(_,n,_) -> n == "AA") taggs)
        endpos = (0, (\(p,_,_) -> p) $ head $ filter (\(_,n,_) -> n == "ZZ") taggs)
        jump1 = mark (foldl setjump1 [m] $ jumps taggs) startpos ([],0)
        jump2 = mark (setjump2 (take 200 $ repeat m) (jumps taggs)) startpos ([],0)

-- part 2
setjump2 :: [Matrix ([Position],Int)] -> [((Int,Int),(Int,Int),TagSide)] -> [Matrix ([Position],Int)]
setjump2 ms js = map (lvljump js) $ zip [0..] ms

lvljump :: [((Int,Int),(Int,Int),TagSide)] -> (Int,Matrix ([Position],Int)) -> Matrix ([Position],Int)
lvljump [] (_,m) = m
lvljump ((from,to,side):js) (lvl,m)
  | lvl == 0 && side == Outer = lvljump js (lvl,m)
  | side == Outer = lvljump js (lvl,head $ mark [m] (0,from) ([(lvl-1,to)],999999))
  | lvl >= 199 = lvljump js (lvl,m)
  | otherwise = lvljump js (lvl,head $ mark [m] (0,from) ([(lvl+1,to)],999999))

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
