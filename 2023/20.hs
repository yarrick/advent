import Data.Char
import Data.List
import qualified Data.Map as M
import Control.DeepSeq
import Debug.Trace

data Pulse = Low | High deriving (Eq, Ord, Show)
instance NFData Pulse where rnf x = seq x ()

data Module = FlipFlop Bool [String] |
              Conj [(String, Pulse)] [String] |
              Broadcast [String] deriving (Eq, Show)
instance NFData Module where
    rnf (FlipFlop st dest) = st `deepseq` dest `deepseq` ()
    rnf (Conj hist dest) = hist `deepseq` dest `deepseq` ()
    rnf x = seq x ()

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

process :: [(String, Module)] -> [String]
process mods = [show $ product $ map length $ group $ sort $ map snd p, show rxticks]
    where ds = groupBy (\a b -> fst a == fst b) $ sort $ concatMap dests mods
          start = (M.fromList $ prep mods ds, [])
          (m,p) = foldl step start (replicate 1000 [("button","broadcaster",Low)])
          rxticks
            | elem "rx" (map fst $ concatMap dests mods) = speedstep (fst start,1)
            | otherwise = 0

prep :: [(String,Module)] -> [[(String,String)]] -> [(String,Module)]
prep [] _ = []
prep ((n,(Conj a dest)):ms) ds = (n,(Conj (map (\t -> (t,Low)) dests) dest)) : prep ms ds
    where dests = map snd $ head $ filter (\d -> fst (head d) == n) ds
prep (m:ms) ds = m : prep ms ds

speedstep :: (M.Map String Module,Int) -> Int
speedstep (mods, it)
    | elem ("rx",Low) np = it
    | it `mod` 100000 == 0 = trace (show it) deepseq (ns,it) $ speedstep (ns,succ it)
    | otherwise = deepseq (ns,it) $ speedstep (ns,succ it)
    where (ns,np) = step (mods,[]) [("button","broadcaster",Low)]

step :: (M.Map String Module, [(String,Pulse)]) -> [(String, String, Pulse)] -> (M.Map String Module, [(String,Pulse)])
step a [] = a
step (mods, past) ((from,to,p):ps)
    | M.notMember to mods = step (mods,(to,p):past) ps
    | otherwise = step (M.insert to nm mods, (to,p):past)  (ps ++ np)
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
