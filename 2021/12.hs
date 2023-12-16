import Data.Char
import Data.List

paths :: [(String,String)] -> String -> [String]
paths ps loc = map dest $ filter (\(a,b) -> a == loc || b == loc) ps
    where dest (a,b)
            | a == loc = b
            | otherwise = a

walk :: ([String] -> String -> Bool) -> [(String,String)] -> [String] -> [[String]]
walk guard ps hist
    | head hist == "end" = [hist]
    | otherwise = concat $ map (\d -> walk guard ps (d:hist)) dirs
    where dirs = filter (guard hist) $ paths ps (head hist)

process :: [String] -> [String]
process rows = map (show.length.(\r -> walk r links ["start"])) [smallOnce, smallTwice]
    where links = map parse rows
          smallOnce hist p = isUpper (head p) || notElem p hist
          smallTwice hist p
            | elem "end" hist || p == "start" = False
            | isUpper (head p) = True
            | notElem p hist = True
            | otherwise = length small == length (nub small)
                where small = filter (\l -> isLower (head l)) hist

parse :: String -> (String, String)
parse str = (a, tail b)
    where (a,b) = break ('-'==) str

main :: IO ()
main = interact (unlines . process . lines)

