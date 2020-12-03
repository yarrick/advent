parse :: String -> [Bool]
parse [] = []
parse (x:xs) = (x == '#') : parse xs

slide :: (Int,Int) -> Int -> Int -> [[Bool]] -> Int
slide _ _ hits [] = hits
slide (angle,speed) x hits slope
    | hit = slide (angle,speed) newx (hits + 1) (drop speed slope)
    | otherwise = slide (angle,speed) newx hits (drop speed slope)
    where t = head slope
          hit = t !! (mod x (length t))
          newx = x + angle

process :: [String] -> [String]
process rows = map show [run (3,1),
                         product $ map run [(1,1), (3,1), (5,1), (7,1), (1,2)]]
    where run v = slide v 0 0 $ map parse rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

