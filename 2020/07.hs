import Data.List

buildColor :: [String] -> String
buildColor ls = concat $ intersperse "_" $ take 2 ls

insides :: [String] -> [(String,Int)]
insides [] = []
insides ("no":end) = []
insides (num:rest) = (buildColor rest, read num) : insides (drop 3 rest)

type Bag = (String,[(String,Int)])

parse :: String -> Bag
parse str = (buildColor tokens, insides $ drop 4 tokens)
    where tokens = words str

isEmpty :: Bag -> Bool
isEmpty (_,[]) = True
isEmpty _ = False

getByColor :: [Bag] -> String -> Bag
getByColor (b@(color,_):bs) name
    | color == name = b
    | otherwise = getByColor bs name

pack :: [Bag] -> (String,Int) -> [(String,Int)]
pack ((bc,contents):bs) (color,num)
    | bc == color = (bc,num) : multBag num contents
    | otherwise = pack bs (color,num)
    where multBag n ((c,nc):bn) = (c,nc*n) : multBag n bn
          multBag n [] = []

combine :: [(String,Int)] -> [(String,Int)]
combine [] = []
combine [a] = [a]
combine (a@(ca,na):b@(cb,nb):cs)
    | ca == cb = combine $ (ca,na+nb):cs
    | otherwise = a : combine (b:cs)

resolve :: [Bag] -> Bag -> (Bool, Bag)
resolve inc (c,contents)
    | not possible = (False, (c,contents))
    | otherwise = (True, (c,filling))
    where known = map fst inc
          possible = all (\x -> elem (fst x) known) contents
          filling = combine $ sort $ concatMap (pack inc) contents

flow :: ([Bag],[Bag]) -> ([Bag],[Bag])
flow (known, other) = (known ++ map snd found, map snd unk)
    where (found,unk) = partition fst $ map (resolve known) other

analyze :: ([Bag],[Bag]) -> [Bag]
analyze (b,[]) = b
analyze bs = analyze $ flow bs

includesColor :: String -> Bag -> Bool
includesColor color (c,contents)
    | c == color = False
    | any (\(cc,_) -> cc == color) contents = True
    | otherwise = False

process :: [String] -> [String]
process rows = map show [length $ filter (includesColor gold) resolved, sum $ map snd $ snd goldBag]
    where bags = map parse rows
          resolved = analyze $ partition isEmpty bags
          gold = "shiny_gold"
          goldBag = getByColor resolved gold

main :: IO ()
main = interact (unlines . process . lines)
