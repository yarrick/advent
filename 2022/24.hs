import Prelude hiding (Left, Right)
import Data.List
import qualified Data.Map as M

data Dir = Up | Right | Down | Left deriving (Eq, Show)

next :: (Int, Int) -> Dir -> (Int, Int)
next (r,c) Up = (r-1,c)
next (r,c) Right = (r,c+1)
next (r,c) Down = (r+1,c)
next (r,c) Left = (r,c-1)

arrow :: Char -> Dir
arrow '^' = Up
arrow '>' = Right
arrow 'v' = Down
arrow '<' = Left

tick :: (Int,Int) -> [((Int,Int),Dir)] -> [((Int,Int),Dir)]
tick _ [] = []
tick (w,h) ((p,dir):cs)
    | dir == Down && nr == h = ((1,nc),dir) : tick (w,h) cs
    | dir == Up && nr == 0 = ((h-1,nc),dir) : tick (w,h) cs
    | dir == Right && nc == w = ((nr,1),dir) : tick (w,h) cs
    | dir == Left && nc == 0 = ((nr,w-1),dir) : tick (w,h) cs
    | otherwise = ((nr,nc),dir) : tick (w,h) cs
    where (nr,nc) = next p dir

comp1 :: ((Int,Int),Int) -> ((Int,Int),Int) -> Ordering
comp1 ((ar,ac),aage) ((br,bc),bage)
    | apos /= bpos = compare bpos apos
    | otherwise = compare bage aage
    where apos = ar + ac
          bpos = br + bc

comp2 :: ((Int,Int),Int) -> ((Int,Int),Int) -> Ordering
comp2 ((ar,ac),aage) ((br,bc),bage)
    | apos /= bpos = compare apos bpos
    | otherwise = compare bage aage
    where apos = ar + ac
          bpos = br + bc

step :: ([M.Map (Int,Int) Dir], (Int,Int), ((Int,Int),(Int,Int)), Int, M.Map ((Int,Int),Int) Bool)
        -> (((Int,Int),Int) -> ((Int,Int),Int) -> Ordering)
        -> [((Int,Int),Int)] -> [((Int,Int),Int)]
step _ _ [] = []
step arg@(blz,(w,h),(start,end),best,visited) cmp ((p,age):ss)
    | age + dist >= best = step arg cmp ss
    | M.member (p,age) visited = step arg cmp ss
    | p == end = (p,age) : step (blz,(w,h),(start,end),age,visited) cmp ss
    | otherwise = step (blz,(w,h),(start,end),best,M.insert (p,age) True visited) cmp $ aged ++ ss
    where blocked = blz !! (succ age)
          dests = p : map (next p) [Up,Right,Down,Left]
          nowall (r,c)
            | r <= 0 || r >= h = elem (r,c) [start,end]
            | otherwise = c > 0 && c < w
          possible = filter (\d -> nowall d && M.notMember d blocked) dests
          aged = sortBy cmp $ zip possible (repeat $ succ age)
          dist = abs (fst end - fst p) + abs (snd end - snd p)

process :: ((Int,Int), Int, Int, [((Int,Int),Dir)]) -> [String]
process ((w,h),startc,endc,cells) = map show [first, solve start end comp1 second]
    where blizz = map M.fromList $ iterate (tick (w,h)) cells
          solve st en cmp t0 = minimum $ map snd $ step (blizz, (w,h), (st,en), 9999, M.empty) cmp [(st,t0)]
          start = (0, startc)
          end = (h, endc)
          first = solve start end comp1 0
          second = solve end start comp2 first

parse :: [String] -> ((Int,Int), Int, Int, [((Int,Int),Dir)])
parse rows = ((length (head rows) - 1, length rows - 1), opening $ head indexed,
              opening $ last indexed, arrows $ concatMap rcs indexed)
    where indexed = zip [0..] $ map (zip [0..]) rows
          opening r = fst $ head $ filter ((=='.').snd) $ snd r
          rcs (r,cs) = map (\(c,ch) -> ((r,c),ch)) cs
          arrows rs = map (\(p,a) -> (p, arrow a)) $ filter (\(_,ch) -> elem ch "v^<>") rs

main :: IO ()
main = interact (unlines . process . parse . lines)
