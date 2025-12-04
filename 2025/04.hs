import Data.Matrix

cull :: Matrix Char -> (Int, Int) -> Char -> Char
cull m p@(rr,cc) cur
    | cur /= '@' = cur
    | cur == '@' && length (active) < 4 = '/'
    | otherwise = '@'
    where around = filter (p/=) [ (r,c) | r <- [rr-1..rr+1], r > 0, r <= nrows m,
                                          c <- [cc-1..cc+1], c > 0, c <= ncols m]
          active = filter (\p -> '@'== (m ! p)) around

process :: Matrix Char -> [String]
process m = map show [orig - (rollcount part1), orig - (rollcount $ part2 m)]
    where rollcount n = length $ filter ('@'==) $ concat $ toLists n
          orig = rollcount m
          part1 = mapPos (cull m) m
          part2 n
            | nn == n = n
            | otherwise = part2 nn
            where nn = mapPos (cull n) n

main :: IO ()
main = interact (unlines . process . fromLists . lines)
