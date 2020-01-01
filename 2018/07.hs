import Data.List
import Data.Char

parse :: [String] -> (Char, Char)
parse ("Step":dep:_:_:_:_:_:start:ss) = (head dep, head start)

solve :: [(String,Char)] -> String
solve [] = []
solve s = goal : solve (map deldep ss)
  where ((deps,goal):ss) = sort s
        deldep (dd,g) = (delete goal dd,g)

tick :: [(String,Int,Char)] -> [(String,Int,Char)]
tick [] = []
tick (dd@(deps,count,g):ss)
  | length deps == 0 && count >= 1 = (deps,count-1,g) : tick ss
  | otherwise = tick ss

clean :: [(String,Int,Char)] -> (String,Int,Char) -> [(String,Int,Char)]
clean goals (_,_,done) = map (\(dd,cc,gg) -> (delete done dd,cc,gg)) goals

tsolve :: Int -> Int -> [(String,Int,Char)] -> [(String,Int,Char)] -> Int
tsolve clock n s working
  | length nextwork == 0 && length worked == 0 = clock
  | otherwise = tsolve (clock+1) n nextwork inprogress
  where ss = sort s
        worked = tick $ working ++ (take (n - length working) ss)
        notworked = drop (length worked - length working) ss
        solved = filter (\(d,count,gg) -> count == 0) worked
        inprogress = filter (\(d,count,gg) -> count > 0) worked
        todo = foldl (\list done -> delete done list) notworked solved
        nextwork = foldl clean todo solved

process :: Int -> Int -> [String] -> [String]
process basetime workers rows = [solve deps, show $ tsolve 0 workers timedeps [] ]
  where steps = map (parse.words) rows
        keys = nub $ sort $ map fst steps ++ map snd steps
        deps = map (\c -> (map fst $ filter (\(_,b) -> c == b) steps,c)) keys
        timedeps = map (\(dd,g) -> (dd,basetime + ord g - ord 'A' + 1,g)) deps

-- long file, lets do IO
main :: IO ()
main = interact (unlines . (process 60 5) . lines)
