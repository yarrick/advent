import qualified Data.Map as M

process  :: [Int] -> [String]
process ns = map (show.sum.M.elems) [elems !! 25, elems !! 75]
    where op m = foldl step M.empty $ M.toList m
          elems = iterate op $ M.fromList $ zip ns (cycle [1])

step :: M.Map Int Int -> (Int, Int) -> M.Map Int Int
step mp (a, reps)
    | a == 0 = put 1 mp
    | even numlen = put m $ put n mp
    | otherwise =  put (a*2024) mp
    where numlen = length (show a)
          (m,n) = divMod a (product $ take (div numlen 2) $ cycle [10])
          put v m = M.insertWithKey (\_ x y -> x + y) v reps m

main :: IO ()
main = interact (unlines . process . map read . words)
