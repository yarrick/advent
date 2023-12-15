import Data.List
import Data.Maybe
import qualified Data.Map as M

-- matched ranges, unmatched ranges, string segment, coming segments
type Candidate = ([Int], [Int], String, [String])

process :: [String] -> [String]
process rows = map show [run 1, run 5]
    where run n = sum $ map (snd.(segment M.empty).parse n) rows

segment :: M.Map Candidate Int -> Candidate -> (M.Map Candidate Int, Int)
segment m cd@(prev,cur,s,ss)
    | cur == [] && s == "" && ss == [] = (m, 1)
    | isJust cached = (m, fromJust cached)
    | otherwise = (M.insert cd combs newm, combs)
    where cached = M.lookup cd m
          gcount cs = map (\a -> (length a, head a)) $ group $ sort cs
          cands = gcount $ concatMap step $ candidates cd
          combinations (cm,cnt) (mul,cand) = (nm, cnt + (mul * combs))
            where (nm,combs) = segment cm cand
          (newm, combs) = foldl combinations (m,0) cands

step :: Candidate -> [Candidate]
step c@(prev,cur,s,ss)
    | length totstring < sum cur = []
    | cur == [] && elem '#' totstring = []
    | cur == [] = [(prev, cur, "", [])]
    | elem '#' s && length s < head cur = []
    | length s < head cur = [(prev, cur, head ss, tail ss)]
    | otherwise = [c]
    where totstring = concat $ s:ss

candidates :: Candidate -> [Candidate]
candidates (prev,len:next,s,ss)
    | elem '#' s && length s < len = []
    | length s < len = [(prev,len:next,s,ss)]
    | slack < 0 = []
    | head s == '#' && length rest > 0 && head rest == '#' = []
    | head s == '#' = found
    | length rest > 0 && head rest == '#' = candidates (prev, len:next, tail s, ss)
    | elem '#' rest && next == [] = candidates (prev, len:next, tail s, ss)
    | elem '#' rest && length rest <= (head next) = candidates (prev, len:next, tail s, ss)
    | otherwise = found ++ candidates (prev, len:next, tail s, ss)
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
