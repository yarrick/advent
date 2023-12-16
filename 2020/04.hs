import Data.List
import Data.Maybe
import Data.Char

grouper :: String -> (String, String)
grouper str = (take c str, drop (c+1) str)
    where c = fromJust $ findIndex (==':') str

parse :: [String] -> [[(String,String)]]
parse [] = []
parse str = map tags ps
    where ps = groupBy (\x y -> (length y) > 0) str
          tags p = map grouper $ concat $ map words p

valid :: [(String,String)] -> Bool
valid tags = length required == length (concat (map hastag required))
    where hastag t = filter (\(a,b) -> a == t) tags
          required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

valid2 :: [(String,String)] -> Bool
valid2 [] = True
valid2 ((t,v):ts)
    | t == "byr" && num >= 1920 && num <= 2002 = valid2 ts
    | t == "byr" = False
    | t == "iyr" && num >= 2010 && num <= 2020 = valid2 ts
    | t == "iyr" = False
    | t == "eyr" && num >= 2020 && num <= 2030 = valid2 ts
    | t == "eyr" = False
    | t == "hgt" && suffix == "cm" && partNum >= 150 && partNum <= 193 = valid2 ts
    | t == "hgt" && suffix == "in" && partNum >= 59 && partNum <= 76 = valid2 ts
    | t == "hgt" = False
    | t == "hcl" && head v == '#' && tail v == filter (\x -> elem x "0123456789abcdef") v = valid2 ts
    | t == "hcl" = False
    | t == "ecl" && elem v ["amb","blu","brn","gry","grn","hzl","oth"] = valid2 ts
    | t == "ecl" = False
    | t == "pid" && 9 == length (filter isDigit v) && length v == 9 = valid2 ts
    | t == "pid" = False
    | otherwise = valid2 ts
    where num = read v
          partNum = read $ takeWhile isDigit v
          suffix = dropWhile isDigit v

process :: [String] -> [String]
process rows = map (show . length) [goodpass, filter valid2 goodpass]
    where goodpass = filter valid $ parse rows

main :: IO ()
main = interact (unlines . process . lines)
