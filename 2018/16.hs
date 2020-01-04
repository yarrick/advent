import Data.Bits
import Data.List

data Operation =
    AddReg | AddImm |
    MulReg | MulImm |
    AndReg | AndImm |
    OrReg  | OrImm  |
    SetReg | SetImm |
    GtIReg | GtRegI | GtRegReg |
    EqIReg | EqRegI | EqRegReg deriving (Eq,Show)

-- read from register (0-based)
gr :: [Int] -> Int -> Int
gr reg idx = reg !! idx

perform :: [Int] -> [Int] -> Operation -> [Int]
perform reg args op = (take pos reg) ++ [res] ++ (drop (pos+1) reg)
  where pos = last args
        res = compute reg (args!!1) (args!!2) op

compute :: [Int] -> Int -> Int -> Operation -> Int
compute regs a b AddReg = compute regs a (gr regs b) AddImm
compute regs a b AddImm = (gr regs a) + b
compute regs a b MulReg = compute regs a (gr regs b) MulImm
compute regs a b MulImm = (gr regs a) * b
compute regs a b AndReg = compute regs a (gr regs b) AndImm
compute regs a b AndImm = (.&.) (gr regs a) b
compute regs a b OrReg = compute regs a (gr regs b) OrImm
compute regs a b OrImm = (.|.) (gr regs a) b
compute regs a b SetReg = compute regs (gr regs a) b SetImm
compute regs a b SetImm = a
compute regs a b GtIReg
  | a > (gr regs b) = 1
  | otherwise = 0
compute regs a b GtRegI
  | (gr regs a) > b = 1
  | otherwise = 0
compute regs a b GtRegReg = compute regs (gr regs a) b GtIReg
compute regs a b EqIReg
  | a == (gr regs b) = 1
  | otherwise = 0
compute regs a b EqRegI
  | (gr regs a) == b = 1
  | otherwise = 0
compute regs a b EqRegReg = compute regs (gr regs a) b EqIReg

allops = [
    AddReg, AddImm, MulReg, MulImm,
    AndReg, AndImm, OrReg, OrImm, SetReg, SetImm,
    GtIReg, GtRegI, GtRegReg, EqIReg, EqRegI, EqRegReg]

matches :: [Int] -> [Int] -> [Int] -> [Operation]
matches inputregs args outputregs = map fst $ filter (\(o,out) -> out == outputregs) results
  where results = map (\op -> (op, perform inputregs args op)) allops

deduce :: [(Int,[Operation])] -> [(Int,Operation)]
deduce [] = []
deduce matching = map (\(val,ops) -> (val, head ops)) known ++ deduce next
  where known = nub $ filter (\(val,ops) -> length ops == 1) matching
        knownops = nub $ concatMap snd known
        next = map (\(val,ops) -> (val, filter (\op -> not $ elem op knownops) ops)) $
               filter (\(val,ops) -> not $ elem val (map fst known)) matching

execute :: [Operation] -> [Int] -> [Int] -> [Int]
execute opmap regs args = perform regs args op
  where op = opmap !! (head args)


parse :: [([Int], [Int], [Int])] -> [String] -> ([([Int],[Int],[Int])],[[Int]])
parse triplets (s:ss)
  | take 6 s == "Before" = parse (triplets++[res]) (drop 3 ss)
  | otherwise = (triplets,map ((map read).words) $ dropWhile (""==) ss)
  where res = (read $ drop 8 s, map read $ words $ head ss, read $ drop 8 (ss!!1))

process (triplets,args) = [show $ length $ filter (>=3) $ map (length.snd) matching,
                           show $ head $ foldl (execute opmap) [0,0,0,0] args]
  where matching = map (\(ir,a,or) -> (head a,matches ir a or)) triplets
        opmap = map snd $ sortBy (\a b -> compare (fst a) (fst b)) $ deduce matching

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (parse []) . lines)
