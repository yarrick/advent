import Data.Char

parse :: String -> (Int,Int)
parse str = (read first, read next)
    where first = takeWhile isDigit str
          next = drop (length first + 1) str

exclude :: (Int,[(Int,Int)]) -> [(Int,Int)] -> [(Int,Int)]
exclude (_,log) parts = filter (/= head log) parts

fitting :: Int -> [(Int,Int)] -> [(Int,Int)]
fitting n parts = filter (\(a,b) -> elem n [a,b]) parts

unused :: (Int,Int) -> Int -> Int
unused (a,b) n
    | a == n = b
    | otherwise = a

build :: (Int, [(Int,Int)]) -> [(Int,Int)] -> [(Int, [(Int,Int)])]
build (need, log) parts = [(need,log)] ++ concatMap (\p -> build p (exclude p parts)) starts
    where choices = fitting need parts
          starts = map (\p -> (unused p need, p:log)) choices

score :: (Int, [(Int,Int)]) -> Int
score (_, log) = sum $ map (\(a,b) -> a + b) log

process :: [(Int,Int)] -> [String]
process parts = map (show.maximum.(map score)) [bridges, filter (\(_,log) -> length log == maxlen) bridges]
    where bridges = build (0,[]) parts
          maxlen = maximum $ map (\(_,log) -> length log) bridges

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map parse) . lines)

