import Data.List

parse :: [String] -> [(Int,[String])]
parse [] = []
parse str = map chunk ps
    where nonempty t = filter (\x -> (length x) > 0) t
          ps = map nonempty $ groupBy (\x y -> (length y) > 0) str
          chunk r = (length r, group $ sort $ concat r)

anyanswer :: (Int,[String]) -> String
anyanswer (_,a) = map head a

allanswer :: (Int,[String]) -> String
allanswer (n,a) = map head $ filter (\x -> (length x) == n) a

process :: [String] -> [String]
process rows = map (show . result) [anyanswer, allanswer]
    where result fun = sum $ map (length . fun) $ parse rows

main :: IO ()
main = interact (unlines . process . lines)
