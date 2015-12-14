
race :: Integer -> (Integer, Integer, Integer) -> Integer
race t (speed, run, rest) = fullruns + curr
	where
		fullruns = full * (speed * run)
		curr = (min run partial) * speed
		(full, partial) = divMod t (run + rest)

reindeers :: [String] -> [(Integer, Integer, Integer)]
reindeers [] = []
reindeers (_:_:_:speed:_:_:run:_:_:_:_:_:_:rest:_:nn) = (read speed, read run, read rest) : reindeers nn

go :: String -> Integer
go str = maximum $ map (race 2503) $ reindeers $ words str

-- part 2

giveScore :: Integer -> [Integer] -> [Integer]
giveScore _ [] = []
giveScore max (a:bb)
	| max == a  = 1 : giveScore max bb
	| otherwise = 0 : giveScore max bb

score :: [Integer] -> [Integer]
score vals = giveScore (maximum vals) vals

addlist :: [Integer] -> [Integer] -> [Integer]
addlist [] _ = []
addlist _ [] = []
addlist (a:aa) (b:bb) = (a+b) : addlist aa bb

fold :: [[Integer]] -> [Integer]
fold (a:[]) = a
fold (a:b:cc) = fold $ (addlist a b):cc

go2 :: String -> Integer
go2 str = maximum $ fold $ map score positions
	where
		positions = [ map (race x) deers | x <- [1..2503]]
		deers = reindeers $ words str
