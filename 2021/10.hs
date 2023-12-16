import Data.List
import Data.Maybe

fetch :: [(Char,a)] -> Char -> a
fetch ts c = fromJust $ lookup c ts

-- Returns (corruption score, incomplete score)
decode :: String -> String -> (Int,Int)
decode log [] = (0, foldl score 0 log)
    where score a c = (5*a) + fetch [('(',1),('[',2),('{',3),('<',4)] c
decode log (c:cs)
    | elem c "([{<" = decode (c:log) cs
    | flipped == head log = decode (tail log) cs
    | otherwise = (fetch [(')',3),(']',57),('}',1197),('>',25137)] c, 0)
    where flipped = fetch [(')','('),(']','['),('}','{'),('>','<')] c

process :: [String] -> [String]
process rows = map show [sum corr, mid $ sort $ filter (>0) inc]
    where (corr,inc) = unzip $ map (decode []) rows
          mid ls = ls !! (div (length ls) 2)

main :: IO ()
main = interact (unlines . process . lines)
