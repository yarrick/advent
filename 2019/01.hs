
fuel :: String -> Integer
fuel weights = sum $ map (calc . read) $ words weights

calc :: Integer -> Integer
calc w = (div w 3) - 2

-- part 2

calc2 :: Integer -> Integer -> Integer
calc2 tot mass
  | fuelmass <= 0 = tot
  | otherwise = calc2 (tot + fuelmass) fuelmass
     where fuelmass = calc mass

fuel2 :: String -> Integer
fuel2 weights = sum $ map (calc2 0 . read) $ words weights

process :: String -> [String]
process rows = map show [fuel rows, fuel2 rows]

main :: IO ()
main = interact (unlines . process)
