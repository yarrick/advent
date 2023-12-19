import Data.Maybe
import Data.List
import Data.Char
import Control.DeepSeq

data Machine = Generator | Chip deriving (Show, Eq, Ord)
instance NFData Machine where rnf x = seq x ()

data Candidate = Result Int
                 | Path (Int, Int, [(Int,[(String,Machine)])], [Move])
                 deriving (Show, Eq, Ord)
instance NFData Candidate where rnf x = seq x ()

isMachine machine (_,m) = machine == m

parts :: [String] -> [(String,Machine)]
parts [] = []
parts ("nothing":xs) = []
parts ("and":xs) = parts xs
parts ("a":kind:thing:xs)
    | take 3 thing == "gen" = (name, Generator) : parts xs
    | take 3 thing == "mic" = (name, Chip) : parts xs
    where name = takeWhile isAlpha kind

parse :: String -> (Int,[(String,Machine)])
parse str = (level, parts $ drop 4 tokens)
    where tokens = words str
          level = fromJust $ findIndex (==(tokens!!1)) ["_","first","second","third","fourth"]

onFloor :: [(Int,[(String,Machine)])] -> Int -> [(String,Machine)]
onFloor state floor = snd $ head $ filter (\(f,contents) -> f == floor) state

machineOnFloor :: [(Int,[(String,Machine)])] -> Int -> Machine -> [String]
machineOnFloor state floor mach = map fst $ filter (isMachine mach) $ onFloor state floor

safeToFloor :: [(Int,[(String,Machine)])] -> Int -> (String,Machine) -> Bool
safeToFloor state floor (kind,Chip)
    | generators == [] = True
    | elem kind generators = True
    | otherwise = False
    where generators = machineOnFloor state floor Generator
safeToFloor state floor (kind,Generator)
    | vulnChips == [] = True
    | vulnChips == [kind] = True
    | otherwise = False
    where generators = machineOnFloor state floor Generator
          chips = machineOnFloor state floor Chip
          vulnChips = filter (\c -> not $ elem c generators) chips

safeFloor :: [(Int,[(String,Machine)])] -> Int -> Bool
safeFloor state f = f >= (minimum floors) && f <= (maximum floors)
    where floors = map fst state

type Move = (Int,Int,[(String,Machine)])
type State = [(Int,[(String,Machine)])]

moves :: State -> Int -> [Move]
moves state floor = [(floor,t,c) | t <- newfloors, c <- uptotwo chunks]
    where newfloors = filter (safeFloor state) [succ floor, pred floor]
          uptotwo xs = map (\x -> [x]) xs ++ [ sort [a,b] | a <- xs, b <- xs, a /= b]
          chunks = onFloor state floor

doMove :: State -> Move -> State
doMove state (from,to,items) = deepseq newState newState
    where delOld (f,things) = (f, filter (\t -> not $ elem t items) things)
          addNew (f,things)
            | f == to = (f, sort $ things ++ items)
            | otherwise = (f,things)
          newState = map addNew $ map delOld state

cullFloors :: State -> State
cullFloors s = dropWhile (\(_,units) -> length units == 0) floors
    where floors = sort s

validRows :: Candidate -> Bool
validRows (Result _) = True
validRows (Path (_,_,rows,_)) = and $ map (validRow.snd) rows
    where gens r = map fst $ filter (isMachine Generator) r
          chips r = map fst $ filter (isMachine Chip) r
          validRow r = length (gens r) == 0 ||
                       all (\k -> elem k (gens r)) (chips r)

advance :: Candidate -> [Candidate]
advance (Result r) = [Result r]
advance (Path (time,floor,state,lastmove)) = filter validRows $ map mover movlist
    where newstate = cullFloors state
          movlist = filter (\x -> not $ elem x lastmove) $ moves newstate floor
          moved s m = cullFloors $ doMove s m
          grade (from,to,units) endstate
            | length endstate == 1 = Result (time+1)
            | otherwise = Path (time+1,to,endstate,(to,from,units) : take 4 lastmove)
          mover (from,to,units) = grade (from,to,units) $ moved newstate (from,to,units)

isResult :: Candidate -> Bool
isResult (Result r) = True
isResult _ = False

step :: [Candidate] -> [Candidate]
step [] = []
step cands@(t:ts)
    | length finished > 0 = [head finished]
    | otherwise = advance t ++ step ts
    where finished = filter isResult cands

numFloors :: Candidate -> Int
numFloors (Result _) = 0
numFloors (Path (_,_,floors,_)) = length floors

same :: Candidate -> Candidate -> Bool
same (Result _) (Result _) = True
same (Result _) (Path _) = False
same (Path _) (Result _) = False
same (Path (_,fa,sa,_)) (Path (_,fb,sb,_)) = fa == fb && sa == sb

score :: Candidate -> Int
score (Result r) = 0
score (Path (_,_,floors,_)) = sum $ map (\(floor, units) -> (5-floor) * length units) floors

run :: [Candidate] -> [Candidate]
run paths
    | (length finished) > 0 = [head finished]
    | otherwise = deepseq stepped stepped
    where finished = filter isResult paths
          minFloors = minimum $ map numFloors paths
          minScore = minimum $ map score paths
          cmpPath x y = compare (score x) (score y)
          closeEnough x = take 10000 $ sortBy cmpPath $ filter (\p -> numFloors p <= (minFloors +1)) x
          stepped = closeEnough $ step $ closeEnough $ nubBy same paths

process :: [String] -> [String]
process rows = map (decode.solve) [state, addStep2 state]
    where state = map parse rows
          addStep2 ((f,s):ss) = (f,s ++ [("elerium",Chip),("elerium",Generator),
                                         ("dilithium",Chip),("dilithium",Generator)]) : ss
          solve s = last $ take 100 $ iterate run [Path (0,1,s,[])]
          decode (Result n:rs) = show n
          decode _ = "Not in <100 moves"

main :: IO ()
main = interact (unlines . process . lines)
