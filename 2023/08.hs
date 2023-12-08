import Data.Char
import Data.List

process (path, route) = map show [walk path route start, foldl1 lcm glens]
    where start = head $ sort $ map fst route
          ghosts = filter (\s -> s !! 2 == 'A') $ map fst route
          glens = map (walk path route) ghosts

walk :: String -> [(String, (String, String))] -> String -> Int
walk _ _ (_:_:'Z':[]) = 0
walk path routes loc = length path + walk path routes (follow path routes loc)

follow :: String -> [(String, (String, String))] -> String -> String
follow [] _ loc = loc
follow (p:ps) routes loc
    | p == 'L' = follow ps routes left
    | otherwise = follow ps routes right
    where (left,right) = snd $ head $ filter (\(g,_) -> g == loc) routes

parse :: [String] -> (String, [(String, (String, String))])
parse rows = (head rows, map (route.tokens) $ drop 2 rows)
    where tokens row = map (filter isAlphaNum) $ words row
          route (a:_:b:c:_) = (a, (b,c))

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

