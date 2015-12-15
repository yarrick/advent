import Data.List
import Data.Char

samenum :: Char -> Char -> Bool
samenum a b
	| b == '-' = False
	| isnum a == isnum b = True
	| otherwise = False
	where isnum c = isDigit c || c == '-'

allnum :: String -> Bool
allnum [] = True
allnum ('-':aa) = allnum aa
allnum (a:bb) = isDigit a && allnum bb

goodnum :: String -> Bool
goodnum "-" = False
goodnum _ = True

numbers :: String -> [String]
numbers str = filter goodnum $ filter allnum $ groupBy samenum str

-- convert " to ' in inputfile to not mess up string argument
solve :: String -> Int
solve str = sum $ map read $ numbers str

-- part 1 only for now..
