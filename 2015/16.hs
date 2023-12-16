
ref :: [(String,Int)]
ref = [("children",3),("cats",7),("samoyeds",2),
    ("pomeranians",3),("akitas",0),("vizslas",0),("goldfish",5),
    ("trees",3),("cars",2),("perfumes",1)]

data Sue = Sue Int [(String,Int)] deriving Show

parse :: [String] -> [Sue]
parse [] = []
parse ("Sue":num:a1:a2:b1:b2:c1:c2:dd) = Sue (getint num) props : parse dd
    where
        substr x = take ((length x) - 1) x
        getint x = read $ substr x
        props = (substr a1, getint a2) : (substr b1, getint b2) : (substr c1, read c2) : []

match :: Sue -> (Int, Int)
match (Sue num (p1:p2:p3:[])) = (num,score p1 + score p2 + score p3)
    where score (prop,val) = rank (lookup prop ref) val

rank :: Maybe Int -> Int -> Int
rank (Just x) y
    | x == y = 1
    | otherwise = 0

allok :: (Int,Int) -> Bool
allok (_,val)
    | val == 3 = True
    | otherwise = False

solve :: [Sue] -> Int
solve sue = fst $ head $ filter allok $ map match sue

-- part 2

score2 :: (String,Int) -> Int
score2 (prop@"cats",val) = rankHi (lookup prop ref) val
score2 (prop@"trees",val) = rankHi (lookup prop ref) val
score2 (prop@"pomeranians",val) = rankLo (lookup prop ref) val
score2 (prop@"goldfish",val) = rankLo (lookup prop ref) val
score2 (prop,val) = rank (lookup prop ref) val

match2 :: Sue -> (Int, Int)
match2 (Sue num (p1:p2:p3:[])) = (num,score2 p1 + score2 p2 + score2 p3)

rankHi :: Maybe Int -> Int -> Int
rankHi (Just x) y
    | y > x = 1
    | otherwise = 0

rankLo :: Maybe Int -> Int -> Int
rankLo (Just x) y
    | y < x = 1
    | otherwise = 0

solve2 :: [Sue] -> Int
solve2 sue = fst $ head $ filter allok $ map match2 sue

process rows = map show [solve parsed, solve2 parsed]
    where parsed = parse $ words rows

main :: IO ()
main = interact (unlines . process)
