import qualified Data.Map as M
import Data.List

process :: [(String, String)] -> [String]
process cs = [show $ length $ filter tee $ M.keys trips, concat $ intersperse "," biggest]
     where join (a,b) m = M.insertWith (++) a [b] $ M.insertWith (++) b [a] m
           connx = foldr join M.empty cs
           trips = foldr (\v m -> M.insert v 1 m) M.empty $ concatMap (triplets connx) $ M.keys connx
           tee ts = elem 't' $ map head ts
           cliques = fst $ bron_kerbosch connx ([],([],M.keys connx,[]))
           biggest = sort $ snd $ last $ sort $ map (\c -> (length c, c)) cliques

triplets :: M.Map String [String] -> String -> [[String]]
triplets m k = map (\(a,b) -> sort [a,b,k]) $ concat shared
    where conns = m M.! k
          shared = [zip (repeat c) $ intersect conns (m M.! c) | c <- conns]

bron_kerbosch m (clq,(r,p,x))
    | p == [] && x == [] = (r:clq,(r,p,x))
    | otherwise = sweep m (clq,(r,p,x)) $ p \\ m M.! (head $ p ++ x)
    where sweep _ rs [] = rs
          sweep m (clq,(r,p,x)) (v:vs) = sweep m (nclq, (r, delete v p, v:x)) vs
               where nb = m M.! v
                     (nclq,(nr,np,nx)) = bron_kerbosch m (clq,((v:r), intersect p nb, intersect x nb))

parse :: String -> (String, String)
parse ss = (a,b)
    where (a,(dash:b)) = break ('-'==) ss

main :: IO ()
main = interact (unlines . process . map parse . lines)
