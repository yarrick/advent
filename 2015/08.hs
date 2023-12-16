overhead :: String -> Int
overhead [] = 0
overhead ('"':as) = 1 + overhead as
overhead ('\\':'\\':as) = 1 + overhead as
overhead ('\\':'"':as) = 1 + overhead as
overhead ('\\':'x':as) = 3 + overhead (drop 2 as)
overhead (a:as) = overhead as

expand :: String -> Int
expand str = length (show str) - length str

process rows = map (show.sum) [map overhead rows, map expand rows]

main :: IO ()
main = interact (unlines . process . lines)
