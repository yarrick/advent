import Data.Char
import Data.List
import qualified Data.Map as M

adjacent :: M.Map [String] [String] -> (M.Map String Int, [[String]]) -> [[String]]
adjacent g (links, cands)
    | length cands == 0 = []
    | otherwise = picked : adjacent g (foldl bump links (g M.! picked),leftover)
    where dist c = M.findWithDefault 0 c links
          adjs = map (\c -> (sum $ map dist c, c)) cands
          (picked,leftover) = top ((-1,[]), [], adjs)
          bump m link = M.insert link (1+M.findWithDefault 0 link m) m

top :: ((Int,[String]), [[String]], [(Int,[String])]) -> ([String], [[String]])
top ((_,a),b,[]) = (a,b)
top ((mx,val),prev,(sc,vv):vs)
    | sc > mx && mx < 0 = top ((sc,vv),prev, vs)
    | sc > mx = top ((sc,vv),val:prev, vs)
    | otherwise = top ((mx,val),vv:prev, vs)

cutphase g (mincut,cutlen)
    | mincut == 3 = (mincut,cutlen)
    | length g > 2 && cut < mincut = cutphase newg (cut,length t)
    | length g > 2 = cutphase newg (mincut,cutlen)
    | otherwise = (mincut,cutlen)
    where (start:rest) = M.keys g
          (t:s:_) = reverse $ adjacent g (M.fromList [ (n,1) | n <- g M.! start ], rest)
          cut = length $ g M.! t
          newvert = concat [t,s]
          newlink = filter (\k -> notElem k newvert) $ concatMap (g M.!) [t,s]
          newg = M.insert newvert newlink $ M.delete t $ M.delete s g

process :: [([String],[String])] -> [String]
process rows = [show $ product [length g - cutlen, cutlen] ]
    where g = M.fromList $ combine $ sort rows
          (cutsize,cutlen) = cutphase g (9999999999,0)

combine :: [([String],[String])] -> [([String],[String])]
combine (a:[]) = [a]
combine (([a],as):([b],bs):cs)
    | a == b = combine (([a],sort $ nub (as++bs)):cs)
    | otherwise = ([a],as) : combine (([b],bs):cs)

parse :: String -> [([String],[String])]
parse str = ([conn],rest) : [ ([r],[conn]) | r <- rest ]
    where (conn:rest) = map (filter isAlpha) $ words str

main :: IO ()
main = interact (unlines . process . concatMap parse . lines)
