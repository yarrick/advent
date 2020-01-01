import Data.List

parse :: Int -> [String] -> [(Int,String,Int,Int)]
parse _ [] = []
parse guard (r:s:rs)
  | wa!!2 == "Guard" = parse (read $ tail $ wa!!3) (s:rs)
  | wa!!2 == "falls" && wb!!2 == "wakes" = (guard,drop 6 (wa!!0), minutes wa, minutes wb) : parse guard rs
  where wa = words r
        wb = words s
        minutes w = read $ take 2 $ drop 3 (w!!1)

sleepguard :: (Int,Int,[(Int,Int)]) -> [(Int,String,Int,Int)] -> [(Int,Int,[(Int,Int)])]
sleepguard a [] = [a]
sleepguard (sleep,guard,periods) ((g,_,from,to):ss)
  | g /= guard && guard < 0 = sleepguard (to-from,g,[(from,to)]) ss
  | g /= guard = (sleep,guard,periods) : sleepguard (to-from,g,[(from,to)]) ss
  | otherwise = sleepguard (sleep+to-from,guard,periods++[(from,to)]) ss

topminute :: [(Int,Int)] -> (Int,Int)
topminute sleeps = last $ sort $ map (\m -> (length m, head m)) $ group sleepmin
  where sleepmin = sort $ concatMap (\(from,to) -> [from..(to-1)]) sleeps

process sleeps = map show [guard * (snd $ topminute periods), (minute*guard2)]
  where sleeplog = sleepguard (0,-1,[]) $ sort sleeps
        (sleeptime,guard,periods) = last $ sort sleeplog
        ((times,minute),guard2) = last $ sort $ map (\(_,g,p) -> (topminute p,g)) sleeplog

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (parse 0) . sort . lines)
