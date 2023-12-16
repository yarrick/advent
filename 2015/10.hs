import Data.List

lookAndSay :: String -> String
lookAndSay str = concatMap (\x -> show (length x) ++ [head x]) $ group str

process (row:_) = map show [stream !! 40, stream !! 50]
    where stream = map length $ iterate lookAndSay row

main :: IO ()
main = interact (unlines . process . lines)
