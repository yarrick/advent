import Data.List
import Data.Maybe
import qualified Data.Map as M

type Block = [(Int,Int)]
type Brick = (Int,Block)

process :: [Block] -> [String]
process rows = map show [length bricks - length required, sum (moves m fallen)]
    where bricks = zip [1..] $ sortBy lowerBlock rows
          (fallen,m) = foldl fall ([], M.empty) bricks
          required = nub $ sort $ concat $ filter (\s -> length s == 1) $ map (supporting m) fallen

fall :: ([Brick], M.Map (Int,Int,Int) Int) -> Brick -> ([Brick], M.Map (Int,Int,Int) Int)
fall (bs,m) b@(n,(x:y:z:[]))
    | any (\(_,_,zz) -> zz == 0) under = (bs++[b],foldl update m full)
    | any (\p -> M.member p m) under = (bs++[b],foldl update m full)
    | otherwise = fall (bs,m) (n,(x:y:(fst z-1,snd z-1):[]))
    where full = [(xb,yb,zb) | xb <- [fst x..snd x], yb <- [fst y..snd y], zb <- [fst z..snd z] ]
          base = filter (\(_,_,zz) -> zz == fst z) full
          under = map (\(x,y,z) -> (x,y,z-1)) base
          update dm p = M.insert p n dm

moves m [] = []
moves m (b:bs) = (differ bs dropped) : moves m bs
    where dropped = disintegrate m b bs
          differ [] b = length b
          differ a [] = length a
          differ (a:as) (b:bs)
            | a == b = differ as bs
            | fst a == fst b = 1 + differ as bs

disintegrate :: M.Map (Int,Int,Int) Int -> Brick -> [Brick] -> [Brick]
disintegrate m target tower = update (erase m target) tower
    where update _ [] = []
          update dm (b:bb) = bs ++ update nm bb
            where (bs,nm) = fall ([], erase dm b) b

erase :: M.Map (Int,Int,Int) Int -> Brick -> M.Map (Int,Int,Int) Int
erase m (n,(x:y:z:[])) = foldl clear m full
    where full = [(xb,yb,zb) | xb <- [fst x..snd x], yb <- [fst y..snd y], zb <- [fst z..snd z] ]
          clear dm p = M.delete p dm

supporting :: M.Map (Int,Int,Int) Int -> Brick -> [Int]
supporting m (n,(x:y:z:[])) = nub $ sort $ map fromJust $ filter isJust $ map (\p -> M.lookup p m) under
    where under = [(xb,yb,zb) | xb <- [fst x..snd x], yb <- [fst y..snd y], zb <- [fst z-1] ]

parse :: String -> Block
parse str = zip (get a) (get $ tail b)
    where (a,b) = break ('~'==) str
          get s = read $ "[" ++ s ++ "]"

lowerBlock :: Block -> Block -> Ordering
lowerBlock b1 b2
    | lowz b1 == lowz b2 = compare b1 b2
    | otherwise = compare (lowz b1) (lowz b2)
    where lowz b = fst $ last b

main :: IO ()
main = interact (unlines . process . map parse . lines)
