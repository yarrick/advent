import Data.List

seatid :: (Int,Int) -> Int
seatid (r,c) = (r * 8) + c

parse :: (Int,Int,Int,Int) -> String -> (Int,Int)
parse (r,_,c,_) [] = (r,c)
parse (lr,hr,lc,hc) (s:ss)
    | s == 'F' = parse (lr,hr-rstep,lc,hc) ss
    | s == 'B' = parse (lr+rstep,hr,lc,hc) ss
    | s == 'L' = parse (lr,hr,lc,hc-cstep) ss
    | s == 'R' = parse (lr,hr,lc+cstep,hc) ss
    where rstep = 1 + div (hr - lr) 2
          cstep = 1 + div (hc - lc) 2

freeseat (u:us) (a:as)
    | u == a = freeseat us as
    | otherwise = a

empty seats = (row, freeseat used [0..7])
    where rows = groupBy (\ (a,b) (c,d) -> a == c) seats
          firstrow = fst $ head seats
          lastrow = fst $ head $ reverse seats
          taken r = (fst $ head r, map snd r)
          notfull = map taken $ filter (\x -> length x /= 8) rows
          (row,used) = head $ filter (\(r,s) -> not $ elem r [firstrow, lastrow]) notfull

process :: [String] -> [String]
process rows = map show [maximum $ map seatid $ seats, seatid $ empty seats]
    where seats = sort $ map (parse (0,127,0,7)) rows

main :: IO ()
main = interact (unlines . process . lines)
