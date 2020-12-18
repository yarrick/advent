import Data.Char
import Data.List
import Data.Matrix

parse :: [String] -> (Int,(Int,Int))
parse rows = (nums !! 0, (succ $ nums !! 2, succ $ nums !! 1))
    where chunks = concatMap (groupBy (\a b -> isDigit a == isDigit b)) rows
          nums = map read $ filter (\x -> isDigit $ head x) chunks

erosion :: Int -> (Int,Int) -> Matrix Int
erosion d (tr,tc) = fromLists $ scanl genRow top [2..(tr+25)]
    where erLvl x = mod (x+d) 20183
          top = map (\x -> erLvl (16807*x)) [0..(tc+84)]
          rowStart r = erLvl (pred r*48271)
          rowCalc r prev v pos
            | r == tr && pos == pred tc = erLvl 0
            | otherwise = erLvl (v*(prev !! pos))
          genRow prev r = scanl (rowCalc r prev) (rowStart r) [1..pred $ length prev]

risk :: Matrix Int -> Matrix Int
risk m = fromLists $ map (map conv) $ toLists m
    where conv x = mod x 3

data Gear = Torch | Shoes | None deriving (Eq,Show,Ord)

adjacent :: Matrix [(Gear,Int)] -> (Int,Int) -> [((Gear,Int),(Int,Int))]
adjacent gm (rr,cc) = concatMap (\p -> zip (gm ! p) (repeat p)) $ updown ++ leftright
    where updown = [(r, cc) | r <- [rr-1,rr+1], r >= 1, r <= nrows gm]
          leftright = [(rr, c) | c <- [cc-1,cc+1], c >= 1, c <= ncols gm]

starter :: Matrix Int -> Matrix [(Gear,Int)]
starter m = matrix (nrows m) (ncols m) setup
    where setup (1,1) = [(Torch, 0)]
          setup _ = []

passable :: Int -> [Gear]
passable 0 = [Torch, Shoes]
passable 1 = [Shoes, None]
passable 2 = [Torch, None]

bestByGear :: [(Gear,Int)] -> [(Gear,Int)]
bestByGear [] = []
bestByGear [a] = [a]
bestByGear (g@(g1,t1):h@(g2,t2):gs)
    | g1 == g2 = bestByGear (g:gs)
    | otherwise = g : bestByGear (h:gs)

step :: Matrix Int -> Matrix [(Gear,Int)] -> (Int,Int) -> Matrix [(Gear,Int)]
step ero path pos
    | length times == 0 = path
    | otherwise = setElem times pos path
    where terrain = ero ! pos
          gear = passable terrain
          arrive ((g,t),to)
            | elem g gear = [(g,t+1)]
            | otherwise = zip allowed (cycle [t+8])
            where allowed = filter (\x -> elem x (passable (ero ! to))) gear
          times = bestByGear $ sort $ (path ! pos) ++ (concatMap arrive $ adjacent path pos)

walk :: Matrix Int -> Int -> Matrix [(Gear,Int)] -> Matrix [(Gear,Int)]
walk em gen gm
    | gm == next = gm
    | otherwise = walk em (succ gen) next
    where cells = [ (r,c) | r <- [1..nrows em], c <- [1..ncols em], r <= gen, r >= (gen - 10)]
          next = foldl (step em) gm cells

process (depth,(tr,tc)) = [show areaRisk, show $ minimum $ map end $ path ! (tr,tc) ]
    where risks = risk $ erosion depth (tr,tc)
          areaRisk = sum $ map (risks !) [(r,c) | r <- [1..tr], c <- [1..tc] ]
          pathStart = starter risks
          path = walk risks 1 pathStart
          end (Torch,t) = t
          end (_,t) = t+7

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
