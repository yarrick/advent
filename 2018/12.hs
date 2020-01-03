
parse :: [String] -> (String, [(String,Char)])
parse rows = (last $ words $ head rows, map splitrow $ drop 2 rows)
  where splitrow str = (take 5 str,last str)

mutate :: [(String,Char)] -> [(Integer,Char)] -> [(Integer,Char)]
mutate xforms row
  | length row < 5 = []
  | length xform > 0 = (idx, snd $ head xform) : mutate xforms (tail row)
  | otherwise = (idx,'.') : mutate xforms (tail row)
  where xform = filter (\(from,to) -> from == (map snd $ take 5 row)) xforms
        idx = fst (row!!2)

pad :: [(Integer,Char)] -> [(Integer,Char)]
pad row = (drop startdots startfillers) ++ row ++ (take (4 - enddots) endfillers)
   where startfillers = reverse $ zip (map (\n -> (fst$head row) - n) [1..4]) (repeat '.')
         startdots = length $ take 4 $ takeWhile (\(_,c) -> c == '.') row
         endfillers = zip (map (\n -> (fst$last row) + n) [1..4]) (repeat '.')
         enddots = length $ take 2 $ takeWhile (\(_,c) -> c == '.') $ reverse row

step :: [(String,Char)] -> [(Integer,Char)] -> [(Integer,Char)]
step xforms row = mutate xforms $ pad row

loop :: [[(Integer,Char)]] -> Integer -> (Integer,Integer,[(Integer,Char)])
loop (a:b:cs) idx
  | map snd a == map snd b = (fst (head b) - fst (head a),idx, a)
  | otherwise = loop (b:cs) (idx+1)

process :: (String, [(String,Char)]) -> [String]
process (s,xforms) = [show $ sum $ map fst $ filter hasplant $ generations !! 20,
                      show $ sum $ map (\(v,_) -> v + (offset * (50000000000 - iteration))) $ filter hasplant stable]
  where hasplant (v,c) = c == '#'
        generations = iterate (step xforms) $ zip [0..] s
        (offset,iteration,stable) = loop generations 0

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
