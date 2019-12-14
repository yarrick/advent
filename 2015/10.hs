import Data.List (group)

lookAndSay :: String -> String
lookAndSay str = concatMap (\x -> show (length x) ++ [head x]) $ group str

run :: String -> [(Int,Int)]
run str = take 51 $ zip [0..] $ map length $ iterate lookAndSay str
