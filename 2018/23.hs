import Data.List

getnum :: String -> [String]
getnum "" = []
getnum str = num : getnum (drop (1+length num) str)
  where num = takeWhile (','/=) str

type Bot = ([Int],Int)

parse :: String -> Bot
parse str = (loc, read $ drop 5 rad)
  where (pos,rad) = break ('>'==) str
        loc = map read $ getnum $ drop 5 pos

dist :: Bot -> Bot -> Int
dist (pa,_) (pb,_) = sum $ map abs $ zipWith (-) pa pb

-- part 2

space :: [(Int,Int)] -> Bot -> [(Int,Int)]
space dims (pos,_) = map expand $ zip pos dims
    where expand (p, (mind,maxd))
            | p < mind = (p,maxd)
            | p > maxd = (mind,p)
            | otherwise = (mind,maxd)

split :: [(Int,Int)] -> [[(Int,Int)]]
split [] = [[]]
split ((a,b):cs)
    | b > a = [ el ++ rest | el <- [[(a,mid)],[(succ mid,b)]], rest <- split cs]
    | otherwise = [ [(a,b)] ++ rest | rest <- split cs ]
    where mid = a + div (b-a) 2

corners :: [(Int,Int)] -> [[Int]]
corners [] = [[]]
corners ((a,b):cs) = [ el : rest | el <- [a,b], rest <- corners cs ]

single r = length (nub $ corners r) == 1

score :: [Bot] -> [(Int,Int)] -> (Int,[(Int,Int)])
score bots rng = (length $ filter id $ map reachable bots,rng)
    where inrange (b,rd) c = dist (b,rd) (c,0) <= rd
          reachable b = any (inrange b) (corners rng)

dig :: [Bot] -> [[(Int,Int)]] -> [(Int,[(Int,Int)])]
dig bots rngs
    | single (head rngs) = map (score bots) rngs
    | otherwise = dig bots $ map snd $ take 64 best
    where best = reverse $ sort $ map (score bots) $ concatMap split rngs

process :: [Bot]  -> [String]
process bots = map show [length $ filter (\x -> x <= snd topbot) $ map (dist topbot) bots,
                         minimum $ map (sum.map abs.head.corners.snd) top]
  where botorder = sortBy (\(_,a) (_,b) -> compare b a) bots
        topbot = head botorder
        cands = dig bots [foldl space [(0,0),(0,0),(0,0)] bots]
        top = filter (\(s,rr) -> s == fst (head cands) && single rr) cands

main :: IO ()
main = interact (unlines . process . (map parse) . lines)
