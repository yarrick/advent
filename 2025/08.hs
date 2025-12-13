import Data.Char
import Data.List
import qualified Data.Map as M

dist ([ax,ay,az], [bx,by,bz]) = sqrt $ fromIntegral diffsquares
    where diffsquares = sum $ map (^2) [ax-bx, ay-by, az-bz]

pairings (a:[]) = []
pairings (a:bs) = [ (a,b) | b <- bs] ++ pairings bs

connect _ cs 0 = cs
connect ((a,b):ds) circmap cables
    | circmap M.! a == circmap M.! b = connect ds circmap (pred cables)
    | otherwise = connect ds (M.map (rekey circnum) circmap) (pred cables)
        where circnum = sort [circmap M.! a, circmap M.! b]
              rekey [new,old] val
                | val == old = new
                | otherwise = val

connect2 ((a,b):ds) circmap
    | length (group $ M.elems nextmap) == 1 = concatMap (take 1) [a,b]
    | otherwise = connect2 ds nextmap
        where circnum = sort [circmap M.! a, circmap M.! b]
              rekey [new,old] val
                | val == old = new
                | otherwise = val
              nextmap = M.map (rekey circnum) circmap

process :: [[Int]] -> [String]
process pos = map (show.product) [ score $ M.toList $ connect distorder circmap cables,
                                   connect2 distorder circmap]
    where distorder = map snd $ sort [ (dist pp, pp) | pp <- pairings pos]
          circmap = M.fromList $ zip pos [1..]
          cables
            | length pos < 50 = 10
            | otherwise = 1000
          prep (p,circ) = (circ,p)
          grouper cs = map length $ group $ map fst $ sort $ map prep cs
          score gs = take 3 $ reverse $ sort $ grouper gs


parse s = map read $ filter (\p -> length p > 1) parts
    where parts = groupBy (\a b -> isDigit a == isDigit b) s

main :: IO ()
main = interact (unlines . process . map parse . lines)
