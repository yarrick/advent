import Data.Char
import Data.List

split :: String -> [String]
split [] = []
split a
 | head a == '[' = (takeWhile notRight a) : split brest
 | otherwise = (takeWhile notLeft a) : split rest
  where notLeft x = x /= '['
        notRight x = x /= ']'
        rest = dropWhile notLeft a
        brest = tail $ dropWhile notRight a

data IpType = Bracketed | Normal deriving (Show, Eq)

bracketed :: [String] -> [(IpType, String)]
bracketed [] = []
bracketed (a:bb)
 | head a == '[' = (Bracketed, tail a) : bracketed bb
 | otherwise = (Normal, a) : bracketed bb

abba :: String -> Bool
abba (a:b:c:[]) = False
abba (a:b:c:d:ee)
 | a == d && b == c && a /= b = True
 | otherwise = abba (b:c:d:ee)

part1 :: String -> Bool
part1 addr
 | or (map snd blist) = False
 | otherwise = or $ map snd nlist
  where list = map (\(a,b) -> (a, abba b)) $ bracketed $ split addr
        blist = filter (\(a,b) -> a == Bracketed) list
        nlist = filter (\(a,b) -> a == Normal) list

process :: [String] -> [String]
process rows = map show [calc part1]
  where calc x = length $ filter (==True) $ map x rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
