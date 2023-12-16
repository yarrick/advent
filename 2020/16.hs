import Data.Char
import Data.List

type Cond = (String, [(Int,Int)])

parseCond :: [String] -> Cond
parseCond (kind:rest) = (kind, map (conv.parts) args)
    where args = filter (\x -> elem '-' x) rest
          parts x = break (=='-') x
          conv (a,b) = (read a, read $ tail b)

parseTicket :: String -> [Int]
parseTicket str = read $ "[" ++ str ++ "]"

inRange :: Int -> Cond -> Bool
inRange val (k,ranges) = any (\(lo,hi) -> lo <= val && hi >= val) ranges

invalid :: [Cond] -> [Int] -> [Int]
invalid conds [] = []
invalid conds (v:vs)
    | length matches == 0 = v : invalid conds vs
    | otherwise = invalid conds vs
    where matches = filter (inRange v) conds

kinds :: [Cond] -> [Int] -> [String]
kinds [] _ = []
kinds (c@(k,_):cs) vals
    | all (\v -> inRange v c) vals = k : kinds cs vals
    | otherwise = kinds cs vals

candCmp :: (Int,[String]) -> (Int,[String]) -> Ordering
candCmp (a,as) (b,bs)
    | length as /= length bs = compare (length as) (length bs)
    | otherwise = compare a b

decide :: [(Int,[String])] -> [(Int,String)]
decide [] = []
decide cands = (k,chosen) : decide nextcs
    where ((k,v):cs) = sortBy candCmp cands
          chosen = head v
          nextcs = map (\(k,s) -> (k, filter (/=chosen) s)) cs

process :: [String] -> [String]
process rows = map show [sum $ invalids, product $ map (\v -> myticket !! v) deps]
    where condRows = takeWhile (/="") rows
          conds = map (parseCond.words) $ map (\(n,str) -> (show n ++ str)) $ zip [0..] condRows
          ticketRows = filter (\x -> length x > 0 && (isDigit $ head x)) $ drop (length condRows) rows
          tickets = map parseTicket ticketRows
          myticket = head tickets
          invalids = invalid conds $ concat tickets
          goodTickets = filter (\x -> invalid conds x == []) tickets
          candidates = map (\(t,vals) -> (t, kinds conds vals)) $ zip [0..] (transpose goodTickets)
          assignments = decide candidates
          deps = map fst $ filter (\(t,name) -> dropWhile isDigit name == "departure") assignments

main :: IO ()
main = interact (unlines . process . lines)
