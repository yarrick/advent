import Data.List (delete, groupBy)
import Data.Maybe (fromJust)

getint :: String -> String -> Int
getint "gain" num = read num
getint "lose" num = -read num

parse :: [String] -> [(String, [(String, Int)])]
parse [] = []
parse (name:_:updown:num:_:_:_:_:_:_:peer:nn) = value : parse nn
    where
        peername = take ((length peer) - 1) peer
        value = (name, [(peername, getint updown num)])

chunk :: [(String, [(String, Int)])] -> [(String, [(String, Int)])]
chunk [] = []
chunk (a:[]) = [a]
chunk (a@(n,x):b@(m,y):cc)
    | n == m = chunk $ (n,x ++ y):cc
    | otherwise = a : chunk (b:cc)

get :: Eq a => [(a, b)] -> a -> b
get list x = fromJust $ lookup x list

xlate :: [(String, Int)] -> [(String, [(String, Int)])] -> [(Int, [(Int, Int)])]
xlate _ [] = []
xlate table ((name,vals):nn) = (get table name, map fixlist vals) : xlate table nn
    where fixlist (peer,val) = (get table peer, val)

translate :: [(String, [(String, Int)])] -> [(Int, [(Int, Int)])]
translate list = xlate table list
    where
        table = zip (map fst list) [0..]

-- cleverness from the internets
permutation :: Eq a => [a] -> [[a]]
permutation []= [[]]
permutation xs = [x:ys | x <-xs, ys <- permutation (delete x xs) ]

evalseat :: [(Int, Int)] -> [Int] -> Int
evalseat vals (left:_:right:_) = get vals left + get vals right

evaltable :: [(Int, [(Int, Int)])] -> [Int] -> Int
evaltable _ (a:b:[])= 0
evaltable list pos = seatscore + evaltable list (tail pos)
    where seatscore = evalseat (get list (pos !! 1)) pos

eval :: [(Int, [(Int, Int)])] -> [Int] -> Int
eval list pos = evaltable list $ take (length list + 2) $ cycle pos

run vals = maximum $ map (eval vals) positions
    where
        samestart a b = head a == head b -- fix one player, switch the rest
        positions = head $ groupBy samestart $ permutation [0..length vals -1]

-- part 2
addme :: [(Int, [(Int, Int)])] -> Int -> [(Int, [(Int, Int)])]
addme [] _ = []
addme ((x,y):zz) me = (x,(me,0):y) : addme zz me


run2 vals = maximum $ map (eval newvals) positions
    where
        me = length vals
        newvals = addme vals me ++ [(me, zip [0..length vals -1] (cycle [0]))]
        samestart a b = head a == head b -- fix one player, switch the rest
        positions = head $ groupBy samestart $ permutation [0..length vals]

process str = map show [run vals, run2 vals]
    where vals = translate $ chunk $ parse $ words str

main :: IO ()
main = interact (unlines . process)
