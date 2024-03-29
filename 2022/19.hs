import Control.DeepSeq
import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Map as M

data Kind = Ore | Clay | Obsidian | Geode | End deriving (Eq,Show,Ord)
instance NFData Kind where rnf k = seq k ()

-- id, ore cost, clay cost, obsidian cost, geode cost
type Costs = (Int, Int, Int, (Int, Int), (Int, Int))

type Count = (Int, Int, Int, Int)

-- costs, robots, resources, minutes
type State = (Costs, Count, Count, Int)

cost :: Costs -> Kind -> Count
cost (_,o,_,_,_) Ore = (o,0,0,0)
cost (_,_,o,_,_) Clay = (o,0,0,0)
cost (_,_,_,(o,c),_) Obsidian = (o,c,0,0)
cost (_,_,_,_,(o,ob)) Geode = (o,0,ob,0)

addbot :: Count -> Kind -> Count
addbot (o,c,ob,g) Ore = (succ o,c,ob,g)
addbot (o,c,ob,g) Clay = (o,succ c,ob,g)
addbot (o,c,ob,g) Obsidian = (o,c,succ ob,g)
addbot (o,c,ob,g) Geode = (o,c,ob,succ g)

hasbot :: Count -> Kind -> Bool
hasbot (_,c,_,_) Clay = c > 0
hasbot (_,_,ob,_) Obsidian = ob > 0
hasbot (_,_,_,g) Geode = g > 0
hasbot _ End = True

produce :: Count -> Count -> Count
produce (bo,bc,bob,bg) (ro,rc,rob,rg) = (bo+ro,bc+rc,bob+rob,bg+rg)

canbuy :: Costs -> Count -> Kind -> Bool
canbuy _ _ End = False
canbuy cs (ho,hc,hob,_) k = ho >= po && hc >= pc && hob >= pob
    where (po,pc,pob,_) = cost cs k

shop :: State -> Kind -> State
shop (cs,bots,(ro,rc,rob,rg),min) k = (cs, addbot bots k,(ro-co,rc-cc,rob-cob,rg),min)
    where (co,cc,cob,_) = cost cs k

worth :: Int -> State -> Kind -> Bool
worth end (_,_,_,min) Geode = min < end
worth end (cs@(_,co,cc,(cobo,cobc),(cgo,cgob)),bots@(bo,bc,bob,_),res@(ro,rc,rob,_),min) k
    | min >= (end-3) && canbuy cs (r23res bots) Geode = False
    | min >= (end-4) && k == Obsidian && canbuy cs (r23res $ addbot bots Obsidian) Geode = True
    | k == Obsidian = min < (end-2)
    | otherwise = min < (end-2)
    where r23res b = (iterate (produce b) res) !! ((end-1) - min)

build :: Int -> (M.Map State Int, Int) -> (State, [Kind]) -> (M.Map State Int, Int)
build end (gm, maxg) (st@(cs,bots,res,min), prevbuy)
    | min == end && geode res > maxg = (gm, geode res)
    | min == end = (gm, maxg)
    | prevbuy == [Ore,Clay] && any (hasbot bots) [Clay,Obsidian,Geode] == False = (gm, maxg)
    | hasbot bots Geode == False && prevbuy == [Ore,Clay,Obsidian] = (gm, maxg)
    | prevmax >= 0 = (gm,maxg)
    | geode res + gprod < maxg = (gm,maxg)
    | otherwise = (M.insert st nmaxg ngm, nmaxg)
    where geode (_,_,_,gs) = gs
          buys = filter (canbuy cs res) [Geode,Obsidian,Ore,Clay]
          newbuys = filter (worth end st) $ filter (\k -> not $ elem k prevbuy) buys
          bought k = (shop (cs,bots,nres,succ min) k, [])
          nres = produce bots res
          nexts = map bought newbuys ++ [((cs,bots,nres,succ min),buys)]
          grank ((_,ab,ar,_),_) ((_,bb,br,_),_) = compare (geode br) (geode ar)
          (ngm,nmaxg) = foldl' (build end) (gm,maxg) $ sortBy grank $ deepseq nexts nexts
          prevmax = M.findWithDefault (-1) st gm
          remains = end - min
          gprod = (remains * geode bots) + 2 * (sum [1..remains])


score :: Int -> State -> [Int]
score end st@((cid,_,_,_,_),_,_,_) = [cid, snd $ build end (M.empty,0) (st,[])]

process :: [Costs] -> [String]
process costs = map show [sum $ map product part1, product $ map last part2]
    where start cs = (cs, (1,0,0,0), (0,0,0,0), 0)
          part1 = map (score 24.start) costs
          part2 = map (score 32.start) $ take 3 costs

parse :: String -> Costs
parse s = (get 1, get 6, get 12, (get 18, get 21), (get 27, get 30))
    where w = map (filter isDigit) $ words s
          get n = read $ w !! n

main :: IO ()
main = interact (unlines . process . map parse . lines)
