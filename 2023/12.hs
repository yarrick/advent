import Data.List
import Data.Maybe
import qualified Data.Map as M

-- matched ranges, unmatched ranges, string segment, coming segments
type Candidate = ([Int], [Int], String, [String])

process :: [String] -> [String]
process rows = map show [run 1, run 5]
    where run n = sum $ map (snd.(segment M.empty).parse n) rows


segment :: M.Map Candidate Int -> Candidate -> (M.Map Candidate Int, Int)
segment m seg@(prev,cur,s,ss)
    | cur == [] && s == "" && ss == [] = (m, 1)
    | isJust cached = (m, fromJust cached)
    | otherwise = (M.insert seg combs newm, combs)
    where cached = M.lookup seg m
          gcount cs = map (\a -> (length a, head a)) $ group $ sort cs
          cands = gcount $ concatMap step $ cand2 seg
          (newm, combs) = foldl combinations (m,0) cands

combinations :: (M.Map Candidate Int, Int) -> (Int, Candidate) -> (M.Map Candidate Int, Int)
combinations (m,cnt) (mul,cand) = (newm, cnt + (mul * combs))
    where (newm,combs) = segment m cand

step :: Candidate -> [Candidate]
step c@(prev,cur,s,ss)
    | length totstring < sum cur = []
    | cur == [] && elem '#' totstring = []
    | cur == [] = [(prev, cur, "", [])]
    | elem '#' s && length s < head cur = []
    | length s < head cur = [(prev, cur, head ss, tail ss)]
    | otherwise = [c]
    where totstring = concat $ s:ss

cand2 :: Candidate -> [Candidate]
cand2 (prev,len:next,s,ss)
    | elem '#' s && length s < len = []
    | length s < len = [(prev,len:next,s,ss)]
    | slack < 0 = []
    | head s == '#' && length rest > 0 && head rest == '#' = []
    | head s == '#' = [(prev++[len],next,drop 1 rest,ss)]
    | length rest > 0 && head rest == '#' = cand2 (prev, len:next, tail s, ss)
    | elem '#' rest && next == [] = cand2 (prev, len:next, tail s, ss)
    | elem '#' rest && length rest <= (head next) = cand2 (prev, len:next, tail s, ss)
    | otherwise = found ++ cand2 (prev, len:next, tail s, ss)
    where (cand,rest) = splitAt len s
          slack = foldl1 (-) $ map (sum . intersperse 1) [map length (s:ss), len:next]
          found = [(prev++[len],next,drop 1 rest,ss)]

parse :: Int -> String -> Candidate
parse n row = ([], concat $ take n numlist , head chunks, drop 1 chunks)
    where (springs:list:[]) = words row
          springlist = intersperse "?" $ cycle [springs]
          numlist = cycle [read $ "[" ++ list ++ "]"]
          chunk [] [] = []
          chunk g [] = [g]
          chunk g (c:as)
            | c == '.' && g /= [] = g : chunk [] as
            | c == '.' = chunk [] as
            | otherwise = chunk (g++[c]) as
          chunks = chunk [] $ concat $ take (pred $ n*2) springlist

main :: IO ()
main = interact (unlines . process . lines)
