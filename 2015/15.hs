import Data.Char (isDigit)

append :: [Int] -> Int -> [Int]
append vals num = vals ++ [num]

terms :: Int -> [Int] -> [[Int]]
terms goal vals = filter (\x -> sum x <= goal) $ map (append vals) [1..left]
    where left = goal - (sum vals)

lastterm :: Int -> [Int] -> [Int]
lastterm goal vals
    | left >= 1 = vals ++ [left]
    | otherwise = []
    where left = goal - (sum vals)

allterms :: Int -> Int -> [[Int]]
allterms x 1 = [[x]]
allterms goal parts = filter notempty $ map (lastterm goal) $ midterms (parts-2) start
    where
        start = terms goal []
        notempty x = length x > 0
        nextterm x = foldl (++) [] $ map (terms goal) x
        midterms num x = (iterate nextterm x) !! num

data Ingredient = Ingredient {
    capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int
} deriving Show

getint :: String -> Int
getint ('-':str) = -1 * getint str
getint str = read $ takeWhile isDigit str

parse :: [String] -> [Ingredient]
parse (_:_:cap:_:dur:_:flav:_:text:_:cal:nn) = ingr : parse nn
    where ingr = Ingredient (getint cap) (getint dur) (getint flav) (getint text) (getint cal)
parse [] = []

negzero :: Int -> Int
negzero x
    | x <= 0 = 0
    | otherwise = x

score :: [Ingredient] -> [Int] -> Int
score ingr spoons = product $ map (sumprop parts) [capacity,durability,flavor,texture]
    where
        parts = zip spoons ingr
        count fn (sp,i) = sp * (fn i)
        sumprop x fn = negzero $ sum $ map (count fn) x

run :: String -> Int
run str = maximum $ map (score ingredients) $ allterms 100 $ length ingredients
    where ingredients = parse $ words str

--part 2
calfilter :: [Int] -> Int
calfilter (cap:dur:fla:tex:cal:[])
    | cal == 500 = product $ cap:dur:fla:tex:[]
    | otherwise = 0

score2 :: [Ingredient] -> [Int] -> Int
score2 ingr spoons = calfilter $ map (sumprop parts) [capacity,durability,flavor,texture,calories]
    where
        parts = zip spoons ingr
        count fn (sp,i) = sp * (fn i)
        sumprop x fn = negzero $ sum $ map (count fn) x

run2 :: String -> Int
run2 str = maximum $ map (score2 ingredients) $ allterms 100 $ length ingredients
    where ingredients = parse $ words str

process rows = map show [run rows, run2 rows]

main :: IO ()
main = interact (unlines . process)
