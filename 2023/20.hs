import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

data Pulse = Low | High deriving (Eq, Ord, Show)

data Module = FlipFlop Bool [String] |
              Conj [(String, Pulse)] [String] |
              Broadcast [String] deriving (Eq, Show)

tick :: Module -> (String, String, Pulse) -> (Module, [(String, String, Pulse)])
tick (Broadcast dest) (from,to,p) = (Broadcast dest, [(to,d,p) | d <- dest])
tick f@(FlipFlop st dest) (from,to,High) = (f,[])
tick (FlipFlop False dest) (from,to,Low) = (FlipFlop True dest, [(to,d,High) | d<- dest])
tick (FlipFlop True dest) (from,to,Low) = (FlipFlop False dest, [(to,d,Low) | d<- dest])
tick (Conj hist dest) (from,to,p)
    | all (High==) (map snd newhist) = (Conj newhist dest, [(to,d,Low) | d <- dest])
    | otherwise = (Conj newhist dest, [(to,d,High) | d <- dest])
    where newhist = (from,p) : (filter (\(s,_) -> s /= from) hist)

dests :: (String, Module) -> [(String,String)]
dests (n, (FlipFlop _ dest)) = map (\d -> (d,n)) dest
dests (n ,(Conj _ dest)) = map (\d -> (d,n)) dest
dests (n, (Broadcast dest)) = map (\d -> (d,n)) dest

rxlow _ _ [] = "Nothing sends to 'rx'"
rxlow mods ((Conj hist dest):[]) (name:[]) = show $ product $ firsthighs sources name mods
    where sources = map fst hist
rxlow _ _ _ = "Module sending to 'rx' not Conjunction type"

firsthighs :: [String] -> String -> M.Map String Module -> [Int]
firsthighs srcs goal mods = map (fst.fromJust.(\s -> find (\(_,ps) -> elem s (map sender ps)) stream)) srcs
    where stream = loops srcs goal (mods,1)
          sender (se,_,_) = se

loops :: [String] -> String -> (M.Map String Module, Int) -> [(Int,[(String,String,Pulse)])]
loops srcs goal (mods, it)
    | length his > 0 = (it,his) : loops srcs goal (ns,succ it)
    | otherwise = loops srcs goal (ns,succ it)
    where (ns,np) = step (mods,[]) [("button","broadcaster",Low)]
          his = filter (\(from,to,p) -> elem from srcs && to == goal && p == High) np

process :: [(String, Module)] -> [String]
process mods = [show $ product pcount, rxlow (fst start) rxmod rxsrc]
    where ds = groupBy (\a b -> fst a == fst b) $ sort $ concatMap dests mods
          start = (M.fromList $ prep mods ds, [])
          (m,p) = foldl step start (replicate 1000 [("button","broadcaster",Low)])
          pcount = map length $ group $ sort $ map (\(_,_,pu) -> pu) p
          rxsrc = map snd $ concat $ filter (\n -> fst (head n) == "rx") ds
          rxmod = map ((fst start) M.!) rxsrc

prep :: [(String,Module)] -> [[(String,String)]] -> [(String,Module)]
prep [] _ = []
prep ((n,(Conj a dest)):ms) ds = (n,(Conj (map (\t -> (t,Low)) dests) dest)) : prep ms ds
    where dests = map snd $ head $ filter (\d -> fst (head d) == n) ds
prep (m:ms) ds = m : prep ms ds

step :: (M.Map String Module, [(String,String,Pulse)]) -> [(String, String, Pulse)] -> (M.Map String Module, [(String,String,Pulse)])
step a [] = a
step (mods, past) ((from,to,p):ps)
    | M.notMember to mods = step (mods,(from,to,p):past) ps
    | otherwise = step (M.insert to nm mods, (from,to,p):past) (ps ++ np)
    where (nm,np) = tick (mods M.! to) (from,to,p)

parse :: String -> (String, Module)
parse row
    | head tname == '&' = (tail tname, Conj [] dest)
    | head tname == '%' = (tail tname, FlipFlop False dest)
    | tname == "broadcaster" = (tname, Broadcast dest)
    where (tname:_:rest) = words row
          dest = map (filter isAlpha) rest

main :: IO ()
main = interact (unlines . process . map parse . lines)
