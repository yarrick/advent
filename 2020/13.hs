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

process :: [String]-> [String]
process (offs:schedule:xs) = [show $ snd $ head waits]
    where offset = read offs
          times = parse schedule
          waits = sort $ map (delay offset) $ filter (>0) times

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
