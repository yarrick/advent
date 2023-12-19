import Data.Char

data Result = Accepted | Rejected | Route String deriving (Eq,Show)
type Obj = [Int]
data Step = Check Int (Int -> Int -> Bool) Int Result | Direct Result
type Func = (String, [Step])
type SuperObj = [(Int,Int)]

process :: ([Func],[Obj]) -> [String]
process (fns,objs) = map (show.sum) [map sum acc1, map (product.combs) acc2]
    where acc1 = passed $ map (exec fns (fetch fns "in")) objs
          passed os = map snd $ filter (\(r,_) -> r == Accepted) os
          start = [(fetch fns "in", replicate 4 (1,4000))]
          acc2 = passed $ concatMap (exec2 fns) start
          combs so = map (\(a,b) -> (b-a)+1) so

exec2 :: [Func] -> ([Step], SuperObj) -> [(Result, SuperObj)]
exec2 _ (_,[(0,0),(0,0),(0,0),(0,0)]) = []
exec2 fns ((Direct (Route r):ss), obj) = exec2 fns (fetch fns r, obj)
exec2 _ ((Direct res:ss), obj) = [(res,obj)]
exec2 fns ((Check pos cmp val res:ss), obj) = (handle res pass) ++ (exec2 fns (ss, fail))
    where (pass,fail) = split pos cmp val obj
          handle (Route s) o = exec2 fns (fetch fns s, o)
          handle a o = [(a,o)]

split :: Int -> (Int -> Int -> Bool) -> Int -> SuperObj -> (SuperObj,SuperObj)
split pos cmp val obj
    | cmp lo val && cmp hi val = (obj, replicate 4 (0,0))
    | cmp lo val == False && cmp hi val == False = (replicate 4 (0,0), obj)
    | cmp lo val && cmp hi val == False = (surr (lo,val-1), surr (val,hi))
    | otherwise = (surr (val+1,hi), surr (lo,val))
    where pre = take pos obj
          post = drop (succ pos) obj
          (lo,hi) = obj !! pos
          surr p = pre ++ [p] ++ post

exec :: [Func] -> [Step] -> Obj -> (Result, Obj)
exec fns (Direct (Route r):ss) obj = exec fns (fetch fns r) obj
exec _ (Direct res:ss) obj = (res,obj)
exec fns (Check pos cmp val res:ss) obj
    | b = handle res
    | otherwise = exec fns ss obj
    where b = cmp (obj !! pos) val
          handle (Route s) = exec fns (fetch fns s) obj
          handle a = (a,obj)

fetch :: [Func] -> String -> [Step]
fetch fns n = snd $ head $ filter (\f -> fst f == n) fns

parse :: [String] -> ([Func], [Obj])
parse rows = (map (func.chunk ",{}") ss,map (map read.chunk "xmas=,{}") $ tail os)
    where (ss,os) = break (""==) rows
          step st
            | length scs == 1 = Direct (result $ head scs)
            | otherwise = Check (gate $ st !! 0) (op $ st !! 1)
                                (read $ drop 2 $ head scs) (result $ last scs)
            where scs = chunk ":" st
          func ss = (head ss, map step $ tail ss)
          gate 'x' = 0
          gate 'm' = 1
          gate 'a' = 2
          gate 's' = 3
          op '<' = (<)
          op '>' = (>)
          result "A" = Accepted
          result "R" = Rejected
          result s = Route s

chunk :: String -> String -> [String]
chunk _ [] = []
chunk sep str = pre : chunk sep (dropWhile match rest)
    where (pre,rest) = break match $ dropWhile match str
          match c = elem c sep

main :: IO ()
main = interact (unlines . process . parse . lines)
