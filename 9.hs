import Data.List (sort, delete)
import Data.Maybe (fromJust)

parse :: [String] -> [(String, [(String, Int)])]
parse [] = []
parse (a:_:b:_:len:pp) = (a,[(b,read len)]):(b,[(a,read len)]):parse pp

chunk :: [(String, [(String, Int)])] -> [(String, [(String, Int)])]
chunk [] = []
chunk (a:[]) = [a]
chunk (prev@(a,b):next@(c,d):ee)
	| a == c = chunk $ (a,b++d):ee
	| otherwise = prev : (chunk $ next:ee)

permutation :: Eq a => [a] -> [[a]]
permutation []= [[]]
permutation xs = [x:ys | x <-xs, ys <- permutation (delete x xs) ]

get :: Eq a => [(a,b)] -> a -> b
get list x = fromJust $ lookup x list

distance :: [(String, [(String,Int)])] -> [String] -> Int
distance _ (a:[]) = 0
distance table (from:to:next) = (get dd to) + distance table (to:next)
	where dd = get table from

-- brute force, 40k combinations
run :: String -> Int
run str = minimum $ map (distance dists) $ permutation $ map fst dists
	where dists = chunk $ sort $ parse $ words str

run2 :: String -> Int
run2 str = maximum $ map (distance dists) $ permutation $ map fst dists
	where dists = chunk $ sort $ parse $ words str
