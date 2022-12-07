import Data.Char
import Data.List

data Item = Dir [String] [Item] Int | File Int deriving (Eq, Show)

isDir (Dir _ _ _) = True
isDir _ = False

size :: Item -> Int
size (Dir _ _ s) = s
size (File s) = s

tree :: Item -> [([String],Item)] -> Item
tree (Dir path _ _) items = (Dir path inside (sum $ map size inside))
    where contains = map snd $ filter (\(p,_) -> p == path) items
          (mydirs, myfiles) = partition isDir contains
          filleddirs = map (\d -> tree d items) mydirs
          inside = filleddirs ++ myfiles

dirsize :: Item -> [Int]
dirsize (File _) = []
dirsize (Dir _ is s) = s : concat (map dirsize is)

process :: [([String],Item)] -> [String]
process rows = map show $ [sum $ filter (<=100000) sizes,
                           head $ filter (>=freeup) $ sort sizes]
    where root = tree (Dir [] [] 0) rows
          sizes = dirsize root
          freeup = 30000000 - (70000000 - (maximum sizes))

parse _ [] = []
parse path ("$ cd ..":ss) = parse (init path) ss
parse path ("$ cd /":ss) = parse [] ss
parse path ("$ ls":ss) = parse path ss
parse path (('$':' ':cmd):ss)
    | take 2 cmd == "cd" = parse (path++[last $ words cmd]) ss
parse path (line:ss)
    | isDigit (head line) = (path, File (read $ head w)) : parse path ss
    | otherwise = (path, Dir (path++[last w]) [] 0) : parse path ss
    where w = words line

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse [] . lines)

