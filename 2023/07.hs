import Data.List
import Data.Maybe

data Type = High | Pair | TwoPair | Three | FullHouse | Four | Five deriving (Eq,Show,Ord)
type Hand = (String,Type)

process :: [(Hand,Int)] -> [String]
process hands = map (show.sum.map score.ranked) [(hands, "23456789TJQKA"), (map joker hands, "J23456789TQKA")]
    where ranked (h, ordr) = zip [1..] $ sortBy (rank ordr) h
          score (r,(_,win)) = r * win

joker ((h,t),win)
    | jokers == 0 || jokers == 5 = ((h,t),win)
    | jokers == 4 = ((h,Five),win)
    | jokers == 3 && t == FullHouse = ((h,Five),win)
    | jokers == 3 = ((h,Four),win)
    | jokers == 2 && t == FullHouse = ((h,Five),win)
    | jokers == 2 && t == TwoPair = ((h,Four),win)
    | jokers == 2 && t == Pair = ((h,Three),win)
    | jokers == 1 && t == Four = ((h,Five),win)
    | jokers == 1 && t == Three = ((h,Four),win)
    | jokers == 1 && t == TwoPair = ((h,FullHouse),win)
    | jokers == 1 && t == Pair = ((h,Three),win)
    | jokers == 1 = ((h,Pair),win)
    where jokers = length $ filter ('J'==) h

rank :: String -> (Hand,Int) -> (Hand,Int) -> Ordering
rank ordr ((as,ah),_) ((bs,bh),_)
    | ah == bh = high ordr as bs
    | otherwise = compare ah bh

high :: String -> String -> String -> Ordering
high _ [] [] = EQ
high ordr (a:as) (b:bs)
    | a == b = high ordr as bs
    | otherwise = compare (order a) (order b)
    where order c = fromMaybe 0 $ elemIndex c ordr

handtype :: String -> Type
handtype hand
    | sets == [5] = Five
    | sets == [1,4] = Four
    | sets == [2,3] = FullHouse
    | sets == [1,1,3] = Three
    | sets == [1,2,2] = TwoPair
    | sets == [1,1,1,2] = Pair
    | sets == [1,1,1,1,1] = High
    where sets = sort $ map length $ group $ sort hand

parse :: String -> (Hand,Int)
parse row = ((hand, handtype hand), read score)
    where (hand:score:_) = words row

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map parse) . lines)

