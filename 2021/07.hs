import Data.List

delta :: [Int] -> [Int] -> Int -> (Int,Int)
delta pos cost goal = (dist, goal)
    where dist = sum $ map (cost !!) $ map abs $ map (goal-) pos

crabcost :: Int -> Int -> [Int]
crabcost last pos = curr : crabcost curr (succ pos)
    where curr = last + pos

run :: [Int] -> [Int]
run pos = [solve [0..], solve (crabcost 0 0)]
    where ds cost = map (delta pos cost) [minimum pos..maximum pos]
          solve cost = fst $ head $ sort $ ds cost

process :: [String] -> [String]
process (row:_) = map show $ run nums
    where nums = read $ "[" ++ row ++ "]"

main :: IO ()
main = interact (unlines . process . lines)
