import Data.List

process (rs,cs,as) = map (show.length.nodes) [[-2,1], [-100..100]]
     where freqs = map (map snd) $ groupBy (\(a,_) (b,_) -> a == b) $ sort as
           nodes rs = nub $ filter valid $ concatMap (harmonics rs) freqs
           valid (r,c) = r >= 0 && r < rs && c >= 0 && c < cs

harmonics _ (a:[]) = []
harmonics rs (a:bs) = concatMap (pairs a) bs ++ harmonics rs bs
    where step (ar,ac) (br,bc) n = (ar+n*(ar-br),ac+n*(ac-bc))
          pairs a b = map (step a b) rs

parse :: [String] -> (Int, Int, [(Char, (Int,Int))])
parse ss = (length ss, length (head ss), filter (\(t,p) -> t /= '.') cells)
    where inject (r,cs) = map (\(c,v) -> (v,(r,c))) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) ss

main :: IO ()
main = interact (unlines . process . parse . lines)
