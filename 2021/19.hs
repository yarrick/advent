import Data.Char
import Data.List

type Scanner = (Int, [(Int,Int,Int)])

process :: [Scanner] -> [String]
process (s:ss) = [show $ length points, show maxdist]
    where ((_,points),scanp,_) = solve (s,[(0,0,0)],ss)
          mdist (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)
          maxdist = maximum [ mdist a b | a <- scanp, b <- scanp ]

solve :: (Scanner,[(Int,Int,Int)],[Scanner]) -> (Scanner,[(Int,Int,Int)],[Scanner])
solve (s,sp,[]) = (s,sp,[])
solve (root,sp,ss) = solve (expand match, newsp:sp, filter (\(sid,_) -> sid /= mid) ss)
    where match@(newsp,_,_,(mid,_)) = head $ concatMap (overlap root) ss

rotations :: [(Int,Int,Int) -> (Int,Int,Int)]
rotations = [id, \(a,b,c) -> (a,c,b), \(a,b,c) -> (b,a,c),
    \(a,b,c) -> (b,c,a), \(a,b,c) -> (c,a,b), \(a,b,c) -> (c,b,a)]

flips :: [(Int,Int,Int) -> (Int,Int,Int)]
flips = [id, \(a,b,c) -> (a,b,-c), \(a,b,c) -> (a,-b,c),
    \(a,b,c) -> (-a,b,c), \(a,b,c) -> (a,-b,-c), \(a,b,c) -> (-a,-b,c),
    \(a,b,c) -> (-a,b,-c), \(a,b,c) -> (-a,-b,-c)]

expand :: ((Int,Int,Int),Int,Scanner,Scanner) -> Scanner
expand (offs,len,(aid,apos),(bid,bpos)) = (aid,allpos)
    where allpos = nub $ apos ++ map (padd offs) bpos
          padd (a,b,c) (d,e,f) = (a+d,b+e,c+f)

overlap :: Scanner -> Scanner -> [((Int,Int,Int),Int,Scanner,Scanner)]
overlap a (n,bpos) = take 1 $ concatMap (shared a) rotb
    where rotb = map (\m -> (n, map m bpos)) [ r.f | r <- rotations, f <- flips ]
          shared a b = map (\n -> (fst$head n,length n,a,b)) $
                       filter (\g -> length g >= 12) $ groupBy same diffs
            where diffs = sort $ concat $ scandiff a b
                  same (a,_) (b,_) = a == b

scandiff :: Scanner -> Scanner -> [[((Int,Int,Int),(Int,Int))]]
scandiff (_,refs) (_,spots) = map (\r -> (distances r) spots) $ zip refs [0..]

distances :: ((Int,Int,Int),Int) -> [(Int,Int,Int)] -> [((Int,Int,Int),(Int,Int))]
distances (pos,n) spots = zip (map (pdist pos) spots) $ zip (repeat n) [0..]
    where pdist (a,b,c) (d,e,f) = (a-d,b-e,c-f)

parse :: [String] -> [Scanner]
parse [] = []
parse rows = (read $ filter isDigit (head curr),
              map coords (tail curr)) : parse (drop 1 rest)
    where (curr,rest) = break (=="") rows

coords :: String -> (Int,Int,Int)
coords str = (read x, read y, read (tail z))
    where (x,yz) = break (==',') str
          (y,z) = break (==',') $ tail yz

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
