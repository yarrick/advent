import qualified Data.Map as M
import Data.List

process :: [(String, String)] -> [String]
process cs = [show $ length $ filter tee $ M.keys trips]
     where join (a,b) m = M.insertWithKey adder a [b] $ M.insertWithKey adder b [a] m
           adder k o n = o ++ n
           connx = foldr join M.empty cs
           trips = foldr (\v m -> M.insert v 1 m) M.empty $ concatMap (triplets connx) $ M.keys connx
           tee ts = elem 't' $ map head ts

triplets :: M.Map String [String] -> String -> [[String]]
triplets m k = map (\(a,b) -> sort [a,b,k]) $ concat shared
    where conns = m M.! k
          shared = [zip (repeat c) $ intersect conns (m M.! c) | c <- conns]

parse :: String -> (String, String)
parse ss = (a,b)
    where (a,(dash:b)) = break ('-'==) ss

main :: IO ()
main = interact (unlines . process . map parse . lines)
