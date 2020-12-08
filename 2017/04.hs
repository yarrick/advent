import Data.List

run :: [String] -> Bool
run parts = length parts == length (nub parts)

run2 :: [String] -> Bool
run2 parts = run $ map sort parts

process :: [String] -> [String]
process rows = map (show.length.filter id) [map run passes, map run2 passes]
    where passes = map words rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

