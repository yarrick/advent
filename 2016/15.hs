import Data.List

check :: [Int] -> Bool
check nums = (last all) < (sum $ take 2 all)
  where all = sort nums

parse :: String -> (Int, Int)
parse str = (read (w !! 3), read $ take (length posstr - 1) posstr)
  where w = words str
        posstr = w !! 11

droptimes :: (Int,Int,Int) -> [Int]
droptimes (delay,size,pos) = filter good [0..]
  where good x = mod (delay+pos+x) size == 0

first :: [[Int]] -> Int
first ds
  | maxdisc == mindisc = maxdisc
  | otherwise = first $ map (dropWhile (maxdisc>)) ds
  where maxdisc = maximum $ map head ds
        mindisc = minimum $ map head ds

part1 :: [String] -> String
part1 rows = show $ first $ map droptimes discs
  where discs = map (\(a,(b,c)) -> (a,b,c)) $ zip [1..] $ map parse rows

part2 :: [String] -> String
part2 rows = show $ first $ map droptimes (discs ++ [(7,11,0)])
  where discs = map (\(a,(b,c)) -> (a,b,c)) $ zip [1..] $ map parse rows

process :: [String] -> [String]
process rows = [part1 rows, part2 rows]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
