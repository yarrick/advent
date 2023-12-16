import Data.Char as Char
import Data.List as List

splitString :: String -> [String]
splitString "" = []
splitString x = w : splitString xx
    where
        w = takeWhile Char.isDigit x
        xx = dropWhile notNum $ drop (length w) x
        notNum c = isSpace c || isAlpha c

parse :: String -> [Integer]
parse str = map read $ splitString str

chunk :: [Integer] -> [[Integer]]
chunk [] = []
chunk ints = sortedList ints : chunk (drop 3 ints)
    where
        sortedList i = List.sort $ take 3 ints

calc :: [Integer] -> Integer
calc (a:b:c:dd) = sideAB3 + sideBC2 + sideAC2
    where
        sideAB3 = 3*a*b
        sideBC2 = 2*b*c
        sideAC2 = 2*a*c

solve :: [String] -> Integer
solve rows = sum $ map calc chunks
    where chunks = chunk $ concatMap parse rows

-- part 2

calcRibbon :: [Integer] -> Integer
calcRibbon (a:b:c:dd) = sides + volume
    where
        sides = 2*(a+b)
        volume = a*b*c

solveRibbon :: [String] -> Integer
solveRibbon rows = sum $ map calcRibbon chunks
    where chunks = chunk $ concatMap parse rows

process rows = map show [solve rows, solveRibbon rows]

main :: IO ()
main = interact (unlines . process . lines)
