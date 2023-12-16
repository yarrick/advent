import Data.Char
import Data.List

parse :: String -> [Integer]
parse [] = []
parse (',':xs) = parse xs
parse ('x':xs) = 0 : parse xs
parse str = (read num) : parse (dropWhile isDigit str)
    where num = takeWhile isDigit str

delay :: Integer -> Integer -> (Integer,Integer)
delay offset t = (t - r, t*(t-r))
    where (q,r) = quotRem offset t

match (cyc,del) n = mod (n+del) cyc == 0

findCyc :: Integer -> (Integer,Integer) -> [Integer]
findCyc b (c,d) = take 2 $ map fst $ filter (\(l,m) -> l > 0 && m == 0) $ map divs muls
    where muls = zip [1..] $ map (*c) [1..]
          divs (p,cc) = quotRem (cc-d) b

line :: [Integer] -> (Integer,Integer)
line (a:b:cs) = (a, b-a)

point :: Integer -> Integer -> [(Integer,Integer)] -> Integer
point base val lines
    | length (nub spots) == 1 = base * newval
    | otherwise = point base newval newlines
    where newval = maximum $ val : (map fst lines)
          step (v,c)
            | v < newval = (v + c *(1 + quot (newval-v-1) c),c)
            | otherwise = (v,c)
          newlines = map step lines
          spots = map fst newlines

combine :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
combine a@(ax,ay) b = (first, point 1 1 [(first+ay,ay), b] - first)
    where first = point 1 1 [a,b]

process :: [String]-> [String]
process (offs:schedule:xs) = map show [snd $ head waits, divisor * (fst $ foldl1 combine lines)]
    where offset = read offs
          times = parse schedule
          waits = sort $ map (delay offset) $ filter (>0) times
          limits = filter (\(a,b) -> a > 0) $ zip times [0..]
          poss t = all (\m -> match m t) limits
          divisor = fst $ head limits
          cycles = map (findCyc divisor) $ tail limits
          lines = map line cycles

main :: IO ()
main = interact (unlines . process . lines)
