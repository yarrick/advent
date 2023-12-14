import Data.List

process :: [String] -> [String]
process rows = map show [build 1, build 5]
    where build n = sum $ concatMap (run.parse n) rows

run ws = map score done ++ concatMap run more
    where end (a,b,c) = a == [] && b == []
          (done, more) = partition end $ paths ws
          score (_,_,w) = w

paths (s:ss, ds, w) = filter valid outs
    where cands = group $ sort $ filter (\p -> not $ elem (-1) (map fst p)) $ candidates ds s sslen
          sslen = sum $ intersperse 1 $ map length ss
          outs = map (\(p:ps) -> (ss, drop (length p) ds, w * length (p:ps))) cands
          valid (pattern,nums,_)
            | elem '#' (concat pattern) && nums == [] = False
            | pattern == [] && length nums > 0 = False
            | otherwise = True

candidates :: [Int] -> String -> Int -> [[(Int,String)]]
candidates [] s _
    | elem '#' s = [[(-1,s)]]
    | otherwise = [[]]
candidates (len:next) s sslen
    | elem '#' s && length s < len = [[(-1,s)]]
    | length s < len = [[]]
    | slack < 0 = [[(-1,s)]]
    | head s == '#' && length rest > 0 && head rest == '#' = [[(-1,s)]]
    | head s == '#' = found
    | length rest > 0 && head rest == '#' = candidates (len:next) (tail s) sslen
    | elem '#' rest && next == [] = candidates (len:next) (tail s) sslen
    | elem '#' rest && length rest <= (head next) = candidates (len:next) (tail s) sslen
    | otherwise = found ++ candidates (len:next) (tail s) sslen
    where (cand,rest) = splitAt len s
          slack = (length s) + 1 + sslen - (sum $ intersperse 1 (len:next))
          found = map ([(len,cand)]++) $ candidates next (drop 1 rest) sslen

parse :: Int -> String -> ([String], [Int], Int)
parse n row = (chunk [] $ concat $ take (pred $ n*2) springlist, concat $ take n numlist, 1)
    where (springs:list:[]) = words row
          springlist = intersperse "?" $ cycle [springs]
          numlist = cycle [read $ "[" ++ list ++ "]"]
          chunk [] [] = []
          chunk g [] = [g]
          chunk g (c:as)
            | c == '.' && g /= [] = g : chunk [] as
            | c == '.' = chunk [] as
            | otherwise = chunk (g++[c]) as

main :: IO ()
main = interact (unlines . process . lines)
