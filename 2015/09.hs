import Data.List
import Data.Maybe

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

process rows = map show [minimum lengths, maximum lengths]
    where dists = chunk $ sort $ parse $ words rows
          lengths = map (distance dists) $ permutation $ map fst dists

main :: IO ()
main = interact (unlines . process)
