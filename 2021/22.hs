import Data.Char
import Data.List
import qualified Data.Map as M

type Pos = (Int,Int,Int)

reboot :: M.Map Pos Int -> (Bool,(Int,Int),(Int,Int),(Int,Int)) -> M.Map Pos Int
reboot mp (st,(xa,xb),(ya,yb),(za,zb))
    | st = M.union mp $ M.fromList $ zip locs (repeat 1)
    | otherwise = foldl (\m p -> M.delete p m) mp locs
    where locs = [(x,y,z) | x <- [xa..xb], y <- [ya..yb], z <- [za..zb]]

process ops = map show [length $ foldl reboot M.empty $ filter tiny ops]
    where tiny (_,(a,b),(c,d),(e,f)) = all (\n -> abs n <=50) [a,b,c,d,e,f]

parse :: String -> (Bool,(Int,Int),(Int,Int),(Int,Int))
parse str = (take 2 (head parts) == "on",(dg 1,dg 3),(dg 5,dg 7),(dg 9,dg 11))
    where isnum c = isDigit c || c == '-'
          parts = groupBy (\a b -> isnum a == isnum b) str
          dg n = read $ parts !! n

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map parse . lines)

