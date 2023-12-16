parse :: String -> [Bool]
parse [] = []
parse (x:xs) = (x == '#') : parse xs

slide :: (Int,Int) -> Int -> Int -> [[Bool]] -> Int
slide _ _ hits [] = hits
slide (angle,speed) x hits slope = slide (angle,speed) (x+angle) newhits (drop speed slope)
    where newhits = hits + (length $ filter id $ [(head slope) !! (mod x (length $ head slope))])

process :: [String] -> [String]
process rows = map show [run (3,1),
                         product $ map run [(1,1), (3,1), (5,1), (7,1), (1,2)]]
    where run v = slide v 0 0 $ map parse rows

main :: IO ()
main = interact (unlines . process . lines)
