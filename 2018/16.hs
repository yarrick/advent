import Device hiding (parse)
import Data.List

allops = [
    AddReg, AddImm, MulReg, MulImm,
    AndReg, AndImm, OrReg, OrImm, SetReg, SetImm,
    GtIReg, GtRegI, GtRegReg, EqIReg, EqRegI, EqRegReg]

matches :: [Int] -> [Int] -> [Int] -> [Operation]
matches inputregs args outputregs = map fst $ filter (\(o,out) -> out == outputregs) results
  where results = map (\op -> (op, perform inputregs (tail args) op)) allops

deduce :: [(Int,[Operation])] -> [(Int,Operation)]
deduce [] = []
deduce matching = map (\(val,ops) -> (val, head ops)) known ++ deduce next
  where known = nub $ filter (\(val,ops) -> length ops == 1) matching
        knownops = nub $ concatMap snd known
        next = map (\(val,ops) -> (val, filter (\op -> not $ elem op knownops) ops)) $
               filter (\(val,ops) -> not $ elem val (map fst known)) matching

execute :: [Operation] -> [Int] -> [Int] -> [Int]
execute opmap regs args = perform regs (tail args) op
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
