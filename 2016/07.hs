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

ababab :: [String] -> String -> Bool
ababab _ (a:b:[]) = False
ababab blist (a:b:c:dd)
 | a == c && isAlpha a && isAlpha b = (inb (b:a:b:[]) ) || ababab blist (b:c:dd)
 | otherwise = ababab blist (b:c:dd)
  where inb str = or $ map (isInfixOf str) blist

part2 :: String -> Bool
part2 addr = or $ map (ababab blist) nlist
  where list = bracketed $ split addr
        blist = map snd $ filter (\(a,b) -> a == Bracketed) list
        nlist = map snd $ filter (\(a,b) -> a == Normal) list

process :: [String] -> [String]
process rows = map show [calc part1, calc part2]
  where calc x = length $ filter (==True) $ map x rows

main :: IO ()
main = interact (unlines . process . lines)
