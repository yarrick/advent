import Data.Matrix

tick :: Matrix Bool -> (Int, Int) -> Bool -> Bool
tick m p@(rr,cc) cur
    | cur && active >= 2 && active <= 3 = True
    | cur == False && active == 3 = True
    | otherwise = False
    where around = filter (p/=) [ (r,c) | r <- [rr-1..rr+1], r > 0, r <= nrows m,
                                          c <- [cc-1..cc+1], c > 0, c <= ncols m]
          active = length $ filter id $ map (\rc -> m ! rc) around

tick2 :: Matrix Bool -> (Int, Int) -> Bool -> Bool
tick2 m p@(r,c) cur
    | r == 1 && c == 1 = True
    | r == 1 && c == ncols m = True
    | r == nrows m && c == 1 = True
    | r == nrows m && c == ncols m = True
    | otherwise = tick m p cur

process rows = map show [after tick 100, after tick2 100]
    where m = fromLists $ map (map ('#'==)) rows
          after fn n = length $ filter id $ toList $ (iterate (stepworld fn) m) !! n
          stepworld fn m = mapPos (fn m) m

main :: IO ()
main = interact (unlines . process . lines)
