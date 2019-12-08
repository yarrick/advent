import Data.List

chunk :: String -> Int -> [String]
chunk [] _ = []
chunk cc size = take size cc : chunk (drop size cc) size

parts :: String -> [(Char, Int)]
parts bytes = [ (head g, length g) | g <- group $ sort bytes ]

run :: String -> Int
run bytes = product $ map snd $ tail best
 where best = head $ sort $ map parts $ chunk bytes 150

-- part 2

decode :: [String] -> String
decode layers
  | sum (map length layers) == 0 = []
  | otherwise = pixel : decode tails
    where
      pixel = head $ filter ('2'/=) $ map head layers
      tails = map tail layers

run2 bytes = unwords $ chunk (decode $ chunk bytes 150) 25
