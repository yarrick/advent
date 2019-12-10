import System.IO

run file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    putStrLn $ process contents
    hClose handle

process :: String -> String
process txt = show $ sum $ map overhead $ lines txt

overhead :: String -> Int
overhead [] = 0
overhead ('"':as) = 1 + overhead as
overhead ('\\':'\\':as) = 1 + overhead as
overhead ('\\':'"':as) = 1 + overhead as
overhead ('\\':'x':as) = 3 + overhead (drop 2 as)
overhead (a:as) = overhead as

-- part 2

run2 file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    putStrLn $ process2 contents
    hClose handle

process2 :: String -> String
process2 txt = show $ sum $ map expand $ lines txt

expand :: String -> Int
expand str = length (show str) - length str
