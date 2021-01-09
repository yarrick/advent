import Data.Matrix
import Data.List

cells m = [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]

neighbors m (rr,cc) = [(r,cc) | r <- [rr-1,rr+1], r >= 1 && r <= (nrows m)] ++
                      [(rr,c) | c <- [cc-1,cc+1], c >= 1 && c <= (ncols m)]

-- pos, kind, health points, attack power
type Unit = ((Int,Int), Char, Int, Int)

units :: Matrix Char -> Int -> [Unit]
units m eap = concatMap fetch $ cells m
    where fetch p
            | elem (m ! p) "#." = []
            | m ! p == 'E' = [(p, 'E', 200, eap)]
            | otherwise = [(p, 'G' , 200, 3)]

-- remove units from map
erase :: Matrix Char -> [Unit] -> Matrix Char
erase m [] = m
erase m ((pos,_,_,_):ps) = erase (setElem '.' pos m) ps

-- draw units on map
draw :: Matrix Char -> [Unit] -> Matrix Char
draw m [] = m
draw m ((pos,k,_,_):ps) = draw (setElem k pos m) ps

flow :: Matrix Int -> [(Int,Int)] -> Matrix Int
flow m [] = m
flow m (pos:ps)
    | self == -1 = flow m ps
    | null steps = flow m ps
    | shortest < self = flow (setElem (succ shortest) pos m) ps
    | otherwise = flow m ps
    where steps = filter (>=0) $ map (m!) $ neighbors m pos
          shortest = minimum steps
          self = m ! pos

spread :: Matrix Int -> Matrix Int
spread m
    | m == next = m
    | otherwise = spread next
    where next = flow m (cells m)

dists :: Matrix Char -> (Int,Int) -> Matrix Int
dists m pos = spread $ setElem 0 pos $ matrix (nrows m) (ncols m) setup
    where setup p
           | m ! p == '.' = 9999
           | otherwise = -1

enemyOf :: Unit -> Unit -> Bool
enemyOf (_,mykind,_,_) (_,theirkind,_,_) = mykind /= theirkind

canAttack :: Matrix Char -> Unit -> [Unit] -> [Unit]
canAttack m me@(pos,_,_,_) troops = filter (\(p,_,_,_) -> elem p adjpos) enemies
    where adjpos = neighbors m pos
          enemies = filter (enemyOf me) troops

distcmp :: ((Int,Int),Int) -> ((Int,Int),Int) -> Ordering
distcmp (p1,d1) (p2,d2)
    | d1 == d2 = compare p1 p2
    | otherwise = compare d1 d2

move :: Matrix Char -> Unit -> [Unit] -> Unit
move clean me@(pos,kind,hp,ap) forces
    | null targets = me
    | null (canAttack clean me forces) = (fst $ head $ sortBy distcmp moves, kind, hp, ap)
    | otherwise = me -- next to enemy already
    where world = draw clean forces
          enemies = filter (enemyOf me) forces
          steps = dists world pos
          goals = map (\p -> (p, steps ! p)) $ concatMap (\(p,_,_,_) -> neighbors world p) enemies
          targets = sortBy distcmp $ filter (\(p,l) -> l >= 0 && l < 9999) goals
          (tgt,dist) = head targets
          tsteps = dists world tgt
          moves = filter (\(p,l) -> l == pred dist) $ map (\p -> (p, tsteps ! p)) $ neighbors world pos

unitcmp :: Unit -> Unit -> Ordering
unitcmp (p1,_,_,_) (p2,_,_,_) = compare p1 p2

playRound :: Matrix Char -> ([Unit],[Unit]) -> ([Unit],[Unit])
playRound _ ([], fought) = ([], sortBy unitcmp fought)
playRound m (f:fs, fought)
    | kinds == 1 = (f:fs, fought)
    | null targets = playRound m (fs, moved:fought)
    | tToFight && thp <= ap = playRound m (excTgt fs, moved:fought)
    | tToFight = playRound m (sortBy unitcmp $ (tp,tk,thp-ap,tap) : excTgt fs, moved:fought)
    | thp <= ap = playRound m (fs, moved : excTgt fought)
    | otherwise = playRound m (fs, moved : (tp,tk,thp-ap,tap) : excTgt fought)
    where kinds = length $ nub $ map (\(_,k,_,_) -> k) (f:fs++fought)
          moved@(_,_,_,ap) = move m f (fs++fought)
          targets = canAttack m moved (fs++fought)
          tcmp (p1,_,hp1,_) (p2,_,hp2,_)
            | hp1 == hp2 = compare p1 p2
            | otherwise = compare hp1 hp2
          tgt@(tp,tk,thp,tap) = head $ sortBy tcmp targets
          tToFight = elem tgt fs
          excTgt fcs = filter (/=tgt) fcs

numElfs :: [Unit] -> Int
numElfs forces = length $ filter (\(_,k,_,_) -> k == 'E') forces

play :: Matrix Char -> Int -> Int -> [Unit] -> (Bool, Int)
play m rnd elfs forces
    | numElfs forces < elfs = (False,0)
    | kinds == 1 = (True, score rnd forces)
    | not $ null left = (True, score rnd (left++played))
    | otherwise = play m (succ rnd) elfs played
    where kinds = length $ nub $ map (\(_,k,_,_) -> k) forces
          (left,played) = playRound m (forces,[])
          score r fs = r * (sum $ map (\(_,_,hp,_) -> hp) fs)

run2 :: Matrix Char -> [[Unit]] -> Int
run2 m (fs:ffs)
    | win = points
    | otherwise = run2 m ffs
    where (win, points) = play m 0 (numElfs fs) fs

process :: Matrix Char -> [String]
process world = map show [snd $ play blank 0 (-1) forces, run2 blank $ map (units world) [4..] ]
    where forces = units world 3
          blank = erase world forces

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . fromLists . lines)
