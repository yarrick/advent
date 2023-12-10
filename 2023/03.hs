import Data.Char
import Data.List
import qualified Data.Matrix as M
import qualified Data.Vector as V

process :: M.Matrix Char -> [String]
process m = map (show . sum) [map partno parts, gears parts]
    where surrounds = map (symboled m) $ concatMap (nums m) [1..M.nrows m]
          parts = filter (\(_,s) -> s /= []) surrounds
          partno ((_, (_, str)), _) = str

gears :: [((Int, (Int, Int)), [((Int, Int), Char)])] -> [Int]
gears vs = map (product . map snd) pairs
    where geared = filter (\(_,sym) -> elem '*' (map snd sym)) vs
          flip (v,ps) = [(fst p, (snd.snd) v) | p <- ps, snd p == '*' ]
          matches = groupBy (\(a,_) (b,_) -> a == b) $ sort $ concatMap flip geared
          pairs = filter (\v -> length v == 2) matches

symboled :: M.Matrix Char -> (Int, (Int, Int)) -> ((Int, (Int, Int)), [((Int, Int), Char)])
symboled m v@(mr, (mc, val)) = (v, symbols)
    where area = [(r, c) | r <- [(mr-1)..(mr+1)], r > 0, r <= M.nrows m,
                           c <- [(mc-1)..(mc+(length $ show val))], c > 0, c <= M.ncols m]
          chars = map (\(r,c) -> ((r,c), M.getElem r c m)) area
          symbols = filter (\(_,c) -> c /= '.') $ filter (not.isDigit.snd) chars

nums :: M.Matrix Char -> Int -> [(Int, (Int, Int))]
nums m row = zip (cycle [row]) $ ngroup (-1) [] digits
    where cells = zip [1..] $ V.toList $ M.getRow row m
          digits = filter (\(_,c) -> isDigit c) cells

ngroup :: Int -> String -> [(Int,Char)] -> [(Int,Int)]
ngroup (-1) _ [] = []
ngroup s d [] = [(s, read d)]
ngroup start ds ((pos,dig):ps)
    | start == -1 = ngroup pos [dig] ps
    | pos == start + (length ds) = ngroup start (ds++[dig]) ps
    | otherwise = (start,read ds) : ngroup pos [dig] ps

main :: IO ()
main = interact (unlines . process . M.fromLists . lines)
