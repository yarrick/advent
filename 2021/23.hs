import Data.List
import Data.Char

data Col = Hallway Char | Room Char String deriving (Eq,Show)

owner c (Hallway _) = False
owner c (Room rc _) = c == rc

allowed _ (Hallway '.') = True
allowed _ (Hallway _) = False
allowed c (Room rc rm)
    | all (==rc) content && c == rc = True
    | otherwise = False
    where content = dropWhile (=='.') rm

solved (Hallway '.') = True
solved (Hallway _) = False
solved (Room rc rm)
    | all (==rc) rm = True
    | otherwise = False

hallpath :: [(Int,Col)] -> (Int -> Int) -> Int -> Char -> [(Int,Col)]
hallpath st fn pos goal
    | next < 0 || next >= length st = []
    | allowed goal loc = (next,loc) : hallpath st fn next goal
    | isAlpha (hallchar loc) = []
    | otherwise = hallpath st fn next goal
    where next = fn pos
          (_,loc) = st !! next
          hallchar (Hallway c) = c
          hallchar (Room _ _) = '_'

trywalk :: [(Int,Col)] -> Int -> Char -> [(Char,(Int,Int))]
trywalk st pos goal
    | length rooms > 0 = [(goal,(pos,fst$head rooms))]
    | isroom $ snd (st !! pos) = map (\(t,_) -> (goal,(pos,t))) targets
    | otherwise = []
    where targets = hallpath st pred pos goal ++ hallpath st succ pos goal
          rooms = filter (isroom.snd) targets
          isroom (Room _ _) = True
          isroom _ = False

mutate :: (Char,(Int,Int)) -> (Int,Col) -> (Int,Col)
mutate (c,(from,to)) (n,Hallway ch)
    | n == from = (n,Hallway '.')
    | n == to = (n,Hallway c)
    | otherwise = (n,Hallway ch)
mutate (c,(from,to)) (n,Room ro rm)
    | n == from = (n,Room ro (space ++ "." ++ (tail cont)))
    | n == to = (n,Room ro (tail space ++ [c] ++ cont))
    | otherwise = (n,Room ro rm)
    where (space,cont) = break isAlpha rm

move :: (Int,[(Int,Col)]) -> (Char,(Int,Int)) -> (Int,[(Int,Col)])
move (cost,st) m@(c,(from,to)) = (cost+newcost, map (mutate m) st)
    where costs = zip "ABCD" $ iterate (10*) 1
          dist = abs (to-from) + roomdist (st !! from) + roomdist (st !! to)
          newcost = dist * snd (head $ filter (\(cc,_) -> cc == c) costs)
          roomdist (n,Hallway _) = 0
          roomdist (n,Room _ rm)
            | n == to = spaces
            | n == from = spaces + 1
            where spaces = length $ takeWhile (=='.') rm

play :: Int -> [(Int,[(Int,Col)])] -> Int
play best [] = best
play best ((cost,st):cs)
    | cost > best = play best cs
    | all (solved.snd) st = play (minimum [best,cost]) cs
    | otherwise = play best (cands++cs)
    where cands = map (move (cost,st)) $ concatMap (canmove st) st

canmove :: [(Int,Col)] -> (Int,Col) -> [(Char,(Int,Int))]
canmove _ (_,Hallway '.') = []
canmove st (n,Hallway c)
    | all (==c) rcontent = trywalk st n c
    | otherwise = []
    where (rp,Room _ rc) = head $ filter (owner c.snd) st
          rcontent = dropWhile (=='.') rc
canmove st (n,Room c pod)
    | all (==c) content = [] -- also matches empty
    | otherwise = trywalk st n (head content)
    where content = dropWhile (=='.') pod

process :: [String] -> [String]
process rows = map (\c -> show$play 99999999 [(0,parse c)]) [rows, deeprows]
    where deeprows = take 3 rows ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ drop 3 rows

parse :: [String] -> [(Int,Col)]
parse rows = zip [0..] $ tgt "ABCD" $ concatMap wrap cols
    where cols = map (filter (\c -> elem c "ABCD.")) $ transpose rows
          wrap [] = []
          wrap [cr] = [Hallway cr]
          wrap room = [Room 'x' $ tail room]
          tgt _ [] = []
          tgt str (Hallway c:cs) = Hallway c : tgt str cs
          tgt (s:ss) (Room _ rs:cs) = Room s rs : tgt ss cs

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

