import Data.List

process (rs,cs,as) = map (show.length)[nodes spots, nodes harmonics]
     where freqs = map (map snd) $ groupBy (\(a,_) (b,_) -> a == b) $ sort as
           nodes fn = nub $ sort $ filter valid $ concatMap fn freqs
           valid (r,c) = r >= 0 && r < rs && c >= 0 && c < cs

spots (a:[]) = []
spots (a:bs) = concatMap (pairs a) bs ++ spots bs
    where pairs (ar,ac) (br,bc) = [(ar+(ar-br),ac+(ac-bc)), (br-(ar-br),bc-(ac-bc))]

harmonics (a:[]) = []
harmonics (a:bs) = concatMap (pairs a) bs ++ harmonics bs
    where step (ar,ac) (br,bc) n = (ar+n*(ar-br),ac+n*(ac-bc))
          pairs a b = map (step a b) [-100..100]

parse :: [String] -> (Int, Int, [(Char, (Int,Int))])
parse ss = (length ss, length (head ss), filter (\(t,p) -> t /= '.') cells)
    where inject (r,cs) = map (\(c,v) -> (v,(r,c))) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) ss

main :: IO ()
main = interact (unlines . process . parse . lines)
