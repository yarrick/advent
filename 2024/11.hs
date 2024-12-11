import qualified Data.Map as M

process  :: [Int] -> [String]
process ns = map (show.sum.(map snd).M.toList) [elems !! 25, elems !! 75]
    where op m = foldl step M.empty $ M.toList m
          elems = iterate op $ M.fromList $ zip ns (cycle [1])

step :: M.Map Int Int -> (Int, Int) -> M.Map Int Int
step mp (a, reps)
    | a == 0 = M.insertWithKey adder 1 reps mp
    | even numlen = M.insertWithKey adder m reps $ M.insertWithKey adder n reps mp
    | otherwise = M.insertWithKey adder (a*2024) reps mp
    where numlen = length (show a)
          (m,n) = divMod a (product $ take (div numlen 2) $ cycle [10])
          adder _ old new = old + new

main :: IO ()
main = interact (unlines . process . map read . words)
