import Data.Maybe (catMaybes)

width :: [Bool] -> Int
width list = round $ sqrt $ fromIntegral $ length list

parse :: String -> [Bool]
parse [] = []
parse (a:bb)
	| a == '.' = False : parse bb
	| a == '#' = True : parse bb
	| otherwise = parse bb

addtrue :: Int -> Bool -> Int
addtrue x False = x
addtrue x True = x + 1

isleftcol :: [Bool] -> Int -> Bool
isleftcol list pos
	| pos `mod` width list == 0 = True
	| otherwise = False

isrightcol :: [Bool] -> Int -> Bool
isrightcol list pos
	| pos `mod` width list == (width list) - 1 = True
	| otherwise = False

istoprow :: [Bool] -> Int -> Bool
istoprow list pos
	| pos `div` width list == 0 = True
	| otherwise = False

isbottomrow :: [Bool] -> Int -> Bool
isbottomrow list pos
	| pos `div` width list == (width list) - 1 = True
	| otherwise = False

getval :: [Bool] -> Int -> ([[Bool] -> Int -> Bool], Int) -> Maybe Bool
getval list mypos (args, pos)
	| filtered || pos < 0 || pos >= (length list) = Nothing
	| otherwise = Just $ list !! pos
	where filtered = maximum $ map (\a -> a list mypos) args


neighbours :: [Bool] -> Int -> Int
neighbours list pos = foldl addtrue 0 $ catMaybes $ map (getval list pos) positions
	where positions =
			[([isleftcol,istoprow],pos - (width list) - 1),
				([istoprow], pos - width list),
				([isrightcol,istoprow], pos - (width list) + 1),
			([isleftcol], pos - 1),
				([isrightcol], pos + 1),
			([isleftcol,isbottomrow], pos + (width list) - 1),
				([isbottomrow], pos + width list),
				([isrightcol,isbottomrow], pos + (width list) + 1)]

step :: [Bool] -> Int -> Bool
step list pos
	| ngbrs == 3 = True
	| state == True && ngbrs == 2 = True
	| otherwise = False
	where
		state = list !! pos
		ngbrs = neighbours list pos

stepworld :: [Bool] -> [Bool]
stepworld list = map (step list) [0..(length list) - 1]

-- very slow (100 minutes on one core..) maybe too much searching in long lists
run :: String -> Int -> Int
run str iter = foldl addtrue 0 reslist
	where
		list = parse str
		reslist = iterate stepworld list !! iter

--part 2


step2 :: [Bool] -> Int -> Bool
step2 list pos
	| pos == 0 = True
	| pos == (width list) -1 = True
	| pos == (length list) - (width list) = True
	| pos == (length list) -1 = True
	| ngbrs == 3 = True
	| state == True && ngbrs == 2 = True
	| otherwise = False
	where
		state = list !! pos
		ngbrs = neighbours list pos

stepworld2 :: [Bool] -> [Bool]
stepworld2 list = map (step2 list) [0..(length list) - 1]

run2 :: String -> Int -> Int
run2 str iter = foldl addtrue 0 reslist
	where
		list = parse str
		reslist = iterate stepworld2 list !! iter
