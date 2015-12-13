import Data.List

nicestr :: String -> Int
nicestr str = length $ filter notCensored $ filter doubleLetter $ filter (minVowels 3) $ words str

minVowels :: Integer -> String -> Bool
minVowels 0 _ = True
minVowels _ [] = False
minVowels x (c:cc)
	| c == 'a' = minVowels (x-1) cc
	| c == 'e' = minVowels (x-1) cc
	| c == 'i' = minVowels (x-1) cc
	| c == 'o' = minVowels (x-1) cc
	| c == 'u' = minVowels (x-1) cc
	| otherwise = minVowels x cc

doubleLetter :: String -> Bool
doubleLetter (a:[]) = False
doubleLetter (a:b:cc)
	| a == b = True
	| otherwise = doubleLetter (b:cc)

notCensored :: String -> Bool
notCensored (a:[]) = True
notCensored (a:b:cc)
	| a:[b] == "ab" = False
	| a:[b] == "cd" = False
	| a:[b] == "pq" = False
	| a:[b] == "xy" = False
	| otherwise = notCensored (b:cc)

-- part 2

hasSubstr :: String -> String -> Bool
hasSubstr _ [] = False
hasSubstr sub str
	| start == sub = True
	| otherwise = hasSubstr sub $ tail str
	where start = take (length sub) str

doublePair :: String -> Bool
doublePair (a:[]) = False
doublePair (a:b:cc)
	| hasSubstr (a:[b]) cc = True
	| otherwise = doublePair (b:cc)

doubleLetterSpaced :: String -> Bool
doubleLetterSpaced (a:b:[]) = False
doubleLetterSpaced (a:b:c:dd)
	| a == c = True
	| otherwise = doubleLetterSpaced (b:c:dd)

nicestr2 :: String -> Int
nicestr2 str = length $ filter doubleLetterSpaced $ filter doublePair $ words str
