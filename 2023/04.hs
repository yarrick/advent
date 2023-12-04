import Data.Char
import Data.List

process :: [([Int], [Int])] -> [String]
process cards = map (show.sum) [map pscore matches, play2 $ zip (cycle [1]) matches]
    where matches = map (\(a,b) -> length $ intersect a b) cards
          pscore 0 = 0
          pscore n = 2 ^ (pred n)

play2 :: [(Int,Int)] -> [Int]
play2 [] = [0]
play2 ((n,ms):ps) = n : (play2 $ (map (\(a,b) -> (a+n,b)) $ take ms ps) ++ (drop ms ps))

parse :: String -> ([Int], [Int])
parse str = (map read wins, map read $ tail nums)
    where (wins,nums) = break ("|"==) $ drop 2 $ words str

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map parse . lines)

