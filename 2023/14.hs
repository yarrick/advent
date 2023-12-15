import Data.List

process :: [String] -> [String]
process rows = map (show.sum) [map score $ zip [1..(length rows)] $ reverse fallen]
    where fallen = transpose $ map fall $ transpose rows
          score (n,s) = n * length (filter ('O'==) s)

fall :: String -> String
fall [] = []
fall s = reverse (sort free) ++ (take 1 stuck) ++ fall (drop 1 stuck)
    where (free,stuck) = break ('#'==) s

main :: IO ()
main = interact (unlines . process . lines)
