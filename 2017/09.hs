import Data.List

data Mode = Normal | Garbage | EscapedGarbage | Escaped deriving (Eq, Show)

stream :: Int -> (Int,Int) -> Mode -> String -> [Int]
stream _ (score,len) _ [] = [score, len]
stream d s Escaped (c:cs) = stream d s Normal cs
stream d s EscapedGarbage (c:cs) = stream d s Garbage cs
stream depth (score,len) Normal (c:cs)
    | c == '<' = stream depth (score,len) Garbage cs
    | c == '{' = stream (succ depth) (score,len) Normal cs
    | c == '}' = stream (pred depth) (score+depth,len) Normal cs
    | c == '!' = stream depth (score,len) Escaped cs
    | otherwise = stream depth (score,len) Normal cs
stream depth (score,glen) Garbage (c:cs)
    | c == '!' = stream depth (score,glen) EscapedGarbage cs
    | c == '>' = stream depth (score,glen) Normal cs
    | otherwise = stream depth (score,glen+1) Garbage cs

process :: String-> [String]
process row = map show $ stream 0 (0,0) Normal row

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process)

