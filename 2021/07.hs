import Data.List

delta :: [Int] -> [Int] -> Int -> (Int,Int)
delta pos cost goal = (dist, goal)
    where dist = sum $ map (cost !!) $ map abs $ map (goal-) pos

crabcost :: Int -> Int -> [Int]
crabcost last pos = curr : crabcost curr (succ pos)
    where curr = last + pos

-- Add brackets around input: 3,4,3,1,2 -> [3,4,3,1,2]
run :: [Int] -> (Int, Int)
run pos = (solve [0..], solve (crabcost 0 0))
    where ds cost = map (delta pos cost) [minimum pos..maximum pos]
          solve cost = fst $ head $ sort $ ds cost
