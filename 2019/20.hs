import Data.Matrix hiding (trace)
import Data.List
import Data.Char
import System.IO

generate :: [String] -> Matrix ([(Int,Int)],Int)
generate input = matrix (length input) (length $ head input) (fill input)

fill :: [String] -> (Int,Int) -> ([(Int,Int)],Int)
fill input (i,j)
  | c == '.' = ([],999999)
  | otherwise = ([],-1)
  where c = (input !! (i-1)) !! (j - 1)

getxy :: Matrix a -> (Int,Int) -> a -> a
getxy m (x,y) fallback
  | x < 0 = fallback
  | y < 0 = fallback
  | x >= nrows m = fallback
  | y >= ncols m = fallback
  | otherwise = getElem (x+1) (y+1) m

mark :: Matrix a -> (Int,Int) -> a -> Matrix a
mark m (x,y) val = setElem val (x+1,y+1) m

update :: Matrix ([(Int,Int)],Int) -> (Int,Int) -> (Matrix ([(Int,Int)],Int),[(Int,Int)])
update m (x,y)
  | self == -1 = (m,[])
  | self == 0 = (m,map fst neighbors)
  | length bestnbr == 0 = (m,[])
  | self <= ndist = (m,[])
  | otherwise = (mark m (x,y) (targets,ndist+1),followups)
  where
    (targets,self) = getxy m (x,y) ([],-1)
    cross = [(x-1,y), (x,y-1), (x+1,y), (x,y+1)] ++ targets
    neighbors = filter (\(_,(_,d)) -> d >= 0) $ zip cross (map (\pos -> getxy m pos ([],-1)) cross)
    bestnbr = sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b) neighbors
    (pos,(tg,ndist)) = head bestnbr
    followups = map fst $ filter (\(_,(_,d)) -> d > (ndist+1)) neighbors

flow :: (Matrix ([(Int,Int)],Int),[(Int,Int)]) -> Matrix ([(Int,Int)],Int)
flow (m,[]) = m
flow (m,(p:ps)) = flow (mm, nub $ ps ++ pps)
  where (mm,pps) = update m p

dropmargin :: [String] -> [String]
dropmargin rows = take (nrow-4) $ drop 2 $ map (take (ncol-4). drop 2) rows
  where nrow = length rows
        ncol = length (rows !! 2)

goodtag :: ((Int,Int),String) -> Bool
goodtag (_,name) = and $ map (\c -> c >= 'A' && c <= 'Z') name

horztags :: [String] -> Int -> [((Int,Int),String)]
horztags rows x = filter goodtag $ map (\(y,str) -> ((x,y),str)) pairs
  where pairs = zip [0..] $ map (\(a,b) -> a:b:[]) $ zip (rows !! 0) (rows !! 1)

verttags :: [String] -> Int -> [((Int,Int),String)]
verttags cols y = filter goodtag $ map (\(x,str) -> ((x,y),str)) pairs
  where pairs = zip [0..] cols

innervtags :: [String] -> [((Int,Int),String)]
innervtags rows = verttags (map (take 2. drop fspace) rows) (fspace-1) ++
  verttags (map (drop (lspace-1).take (lspace+1)) rows) (lspace+1)
  where numrows = map (zip [0..]) rows
        spaces = map fst $ filter (\(_,c) -> c == ' ') $ concat numrows
        fspace = minimum spaces
        lspace = maximum spaces

innerhtags rows = horztags (map snd upper) ((fst$head upper)-1) ++
  horztags (map snd lower) ((fst$last lower)+1)
  where midrows = filter (\r -> elem ' ' (snd r)) $ zip [0..] rows
        upper = take 2 midrows
        lower = drop (length midrows-2) midrows

innertags :: [String] -> [((Int,Int),String)]
innertags rows = innervtags rows ++ innerhtags rows

tags :: [String] -> [((Int,Int),String)]
tags rows = horztags (map (drop 2) $ take 2 rows) 0 ++ horztags (map (drop 2) $ drop (nrow-2) rows) (nrow-5) ++
  verttags (drop 2 $ map (take 2) rows) 0 ++ verttags (drop 2 $ map (drop $ ncol-2) rows) (ncol-5) ++ innertags (dropmargin rows)
  where nrow = length rows
        ncol = length (rows !! 2)

jumps :: [((Int,Int),String)] -> [((Int,Int),(Int,Int))]
jumps tgs = pairs
  where pairs = map (\((a,_),(b,_)) -> (a,b)) $ filter (\((_,a),(_,b)) -> a == b) [ (a,b) | a <- tgs, b <- (delete a tgs) ]

setjump :: Matrix ([(Int,Int)],Int) -> ((Int,Int),(Int,Int)) -> Matrix ([(Int,Int)],Int)
setjump m (from,to)= mark m from ([to],999999)

process :: [String] -> [String]
process rows = [show $ snd $ getxy (flow (jumpm,[startpos])) endpos ([],-1)]
  where m = generate $ dropmargin rows
        taggs = tags rows
        startpos = fst $ head $ filter (\(_,n) -> n == "AA") taggs
        endpos = fst $ head $ filter (\(_,n) -> n == "ZZ") taggs
        jumpm = mark (foldl setjump m $ jumps taggs) startpos ([],0)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
