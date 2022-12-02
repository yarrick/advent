import Data.List

play :: (Char, Char) -> Int
play ('A', 'X') = 1 + 3
play ('A', 'Y') = 2 + 6
play ('A', 'Z') = 3 + 0
play ('B', 'X') = 1 + 0
play ('B', 'Y') = 2 + 3
play ('B', 'Z') = 3 + 6
play ('C', 'X') = 1 + 6
play ('C', 'Y') = 2 + 0
play ('C', 'Z') = 3 + 3

play2 :: (Char, Char) -> Int
play2 ('A', 'X') = 0 + 3
play2 ('A', 'Y') = 3 + 1
play2 ('A', 'Z') = 6 + 2
play2 ('B', 'X') = 0 + 1
play2 ('B', 'Y') = 3 + 2
play2 ('B', 'Z') = 6 + 3
play2 ('C', 'X') = 0 + 2
play2 ('C', 'Y') = 3 + 3
play2 ('C', 'Z') = 6 + 1

process :: [String] -> [String]
process rows = map (\p -> show $ sum $ map p bouts) [play, play2]
    where bouts = map (\r -> (r !! 0, r !! 2)) rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

