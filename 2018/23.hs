import Data.List

getnum :: String -> [String]
getnum "" = []
getnum str = num : getnum (drop (1+length num) str)
  where num = takeWhile (','/=) str

type Bot = ([Int],Int)

parse :: String -> Bot
parse str = (loc, read $ drop 5 rad)
  where (pos,rad) = break ('>'==) str
        loc = map read $ getnum $ drop 5 pos

dist :: Bot -> Bot -> Int
dist (pa,_) (pb,_) = sum $ map abs $ zipWith (-) pa pb

process :: [Bot]  -> [String]
process bots = [show $ length $ filter (\x -> x <= snd topbot) $ map (dist topbot) bots]
  where botorder = sortBy (\(_,a) (_,b) -> compare b a) bots
        topbot = head botorder

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map parse) . lines)
