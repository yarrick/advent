import Data.Char
import Data.List

stepd :: [(String, [(String, Int)])] -> [(String, [(String, Int)])] -> [(String, [(String, Int)])]
stepd [] _ = []
stepd ((c,dists):ds) pdist = (c,shortest) : stepd ds pdist
    where paths = map fst $ filter (\(_,d) -> d == 1) dists
          reachable = concat $ map snd $ filter (\(r,_) -> elem r paths) pdist
          newdists = dists ++ map (\(r,cost) -> (r,succ cost)) reachable
          sameroom (a,_) (b,_) = a == b
          shortest = filter (\(r,_) -> r /= c) $ map head $ groupBy sameroom $ sort newdists

calcdist :: [(String, [(String, Int)])] -> [(String, [(String, Int)])]
calcdist dists
    | minlen + 1 == length dists = dists
    | otherwise = calcdist $ stepd dists dists
    where minlen = minimum $ map (length.snd) dists

dist :: [(String, [(String, Int)])] -> String -> String -> Int
dist dists from to = snd $ head $ filter (\(rr,_) -> rr == to) roomdist
    where roomdist = snd $ head $ filter (\(r,_) -> r == from) dists

pick :: [(String, [(String, Int)])] -> [(String,Int)] -> [(String,Int)] -> (String, Int) -> [[(String, Int)]]
pick dists valves hist (room,time)
    | valves == [] = [hist]
    | time < 0 = []
    | otherwise = hist : concatMap (\t -> pick dists (otherv t) ((t,score t):hist) (t,rest t)) targets
    where flow v = snd $ head $ filter (\(vv,_) -> v == vv) valves
          rest v = (time - (dist dists room v) - 1)
          score v = flow v * rest v
          otherv v = filter (\(vv,_) -> v /= vv) valves
          targets = map fst valves

duopath :: Int -> Int -> [[(Int,[String])]] -> [Int]
duopath vs score (mp:ps)
    | best == [] = []
    | otherwise = (last best) : duopath vs (last best) ps
    where pathlen p = maximum $ map (length.snd) p
          mylen = pathlen mp
          valid = concat $ filter (\g -> pathlen g + mylen <= vs) ps
          mix = [(av+bv, ap++bp) | (av,ap) <- mp, (bv,bp) <- valid,
                 (av+bv) > score && intersect ap bp == [] ]
          best = sort $ map fst mix

process :: [(String,(Int,[String]))] -> [String]
process rows = map (show.maximum) [map score paths, duopath (length valves) 0 slens]
    where dists = calcdist $ map (\(c,(_,r)) -> (c, zip r $ repeat 1)) rows
          valves = map (\(r,(v,_)) -> (r,v)) $ filter (\(_,(v,_)) -> v > 0) rows
          paths = pick dists valves [] ("AA", 30)
          score p = sum $ map snd p
          epaths = pick dists valves [] ("AA", 26)
          sides = map (\p -> (score p, sort $ map fst p)) epaths
          pathlen (_,p) = length p
          slens = groupBy (\a b -> pathlen a == pathlen b) $
                  sortBy (\a b -> compare (pathlen a) (pathlen b)) sides

parse :: String -> (String,(Int,[String]))
parse s = (w !! 1, (read $ filter isDigit $ w !! 4, map (filter isAlpha) $ drop 9 w))
    where w = words s

main :: IO ()
main = interact (unlines . process . map parse . lines)
