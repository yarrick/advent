import Data.List
import qualified Data.Map as M

neighbors :: [Int] -> [[Int]]
neighbors (x:y:z:[]) = [[x-1,y,z],[x+1,y,z],[x,y-1,z],[x,y+1,z],[x,y,z-1],[x,y,z+1]]

contact :: M.Map [Int] Bool -> [Int] -> Int
contact om pos = length $ filter (\p -> M.member p om) $ neighbors pos

freesides :: M.Map [Int] Bool -> [Int] -> Int
freesides mp pos = 6 - contact mp pos

dims :: [[Int]] -> (Int,Int)
dims ps = (minimum mins - 1, maximum maxs + 1)
    where (mins,maxs) = unzip $ map (\p -> (minimum p, maximum p)) $ transpose ps

flow :: M.Map [Int] Bool -> (Int,Int) -> [[Int]] -> M.Map [Int] Bool -> M.Map [Int] Bool
flow _ _ [] o = o
flow cs (minv,maxv) (pos:pps) omap
    | length found == 0 = flow cs (minv,maxv) pps omap
    | otherwise = flow cs (minv,maxv) (pps++found) (foldl mark omap found)
    where inbounds coord = all (\c -> c >= minv && c <= maxv) coord
          cands = filter (\p -> M.notMember p cs && inbounds p) $ neighbors pos
          found = filter (\p -> M.notMember p omap) cands
          mark mpp p = M.insert p True mpp

process :: [[Int]] -> [String]
process cs = map (show.sum) [map (freesides m) cs, map (contact outer) cs]
    where m = M.fromList $ zip cs (cycle [True])
          bounds = dims cs
          startpos = replicate 3 (fst bounds)
          outer = flow m bounds [startpos] (M.singleton startpos True)


parse :: String -> [Int]
parse s = read ("["++s++"]")

main :: IO ()
main = interact (unlines . process . map parse . lines)
