import Data.Char
import Data.List

reacts :: Char -> Char -> Bool
reacts a b = a /= b && (toUpper a == b || toUpper b == a)

chain :: String -> String -> String
chain [] (s:ss) = chain [s] ss
chain a [] = reverse a
chain prev (n:ns)
  | reacts (head prev) n = chain (tail prev) ns
  | otherwise = chain (n:prev) ns

process :: String -> [String]
process str = map show [postlen str, minimum lengths]
  where postlen s = length $ chain [] s
        chars = nub $ sort $ map toLower str
        lengths = map (\c -> postlen $ filter (\x -> toLower x /= c) str) chars

main :: IO ()
main = interact (unlines . (concatMap process) . lines)
