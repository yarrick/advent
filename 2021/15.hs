import Data.Char
import Data.List
import Data.Matrix
import qualified Data.Map as M

type Pos = (Int, Int)

getxy :: Matrix a -> a -> Pos -> a
getxy m fallback (x,y)
  | x <= 0 || y <= 0 || x > nrows m || y > ncols m = fallback
  | otherwise = getElem x y m

next :: Matrix a -> M.Map Pos Int -> Pos -> [(Pos, Int)]
next m a (r,c) = map (\pos -> (pos, M.findWithDefault 99999 pos a)) valid
    where cands = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]
          valid = filter (\(r,c) -> r >= 1 && r <= nrows m && c >= 1 && c <= ncols m) cands

update :: Matrix Int -> Pos -> (M.Map Pos Int, [Pos], Int) -> (M.Map Pos Int, [Pos], Int)
update a _ (b,[],n) = (b,[],n)
update rs target (ss,pos:cs,curr)
    | pos == target && newsum < tval = (M.insert pos newsum ss, [], newsum)
    | newsum < 99999 && newsum < cursum = update rs target (M.insert pos newsum ss, sortBy lower $ cs ++ npos, curr)
    | otherwise = update rs target (ss, cs, curr)
    where rval = rs ! pos
          (npos,nvals) = unzip $ next rs ss pos
          cursum = M.findWithDefault 99999 pos ss
          newsum = rval + minimum nvals
          tval = M.findWithDefault 99999 target ss
          lower a b = compare (prio a) (prio b)
          prio (a,b) = (M.findWithDefault 99999 (a,b) ss) + abs ((fst target) - a) + abs ((snd target) - b)

solve :: Matrix Int -> Int
solve starter = score
    where risk = setElem 0 (1,1) starter
          rs = nrows risk
          cs = ncols risk
          sums = M.fromList [((rs,cs), risk ! (rs,cs))]
          (summed,_,score) = update risk (1,1) (sums, map fst $ next risk sums (rs,cs), 99999)

-- messier due to 1-based indexing
grow :: Matrix Int -> Matrix Int
grow g = matrix (5*rs) (5*cs) copier
    where rs = nrows g
          cs = ncols g
          copier pos@(r,c)
            | r <= rs && c <= cs = getxy g (-5) pos
            | otherwise = clamp $ getxy g (-9) (wrap r rs,wrap c cs) + steps
            where steps = (div (r-1) rs) + (div (c-1) cs)
                  wrap p len = 1 + (p-1) `mod` len
                  clamp n
                    | n <= 9 = n
                    | otherwise = mod n 9

process :: [String] -> [String]
process rows = map show [solve m, solve $ grow m]
  where m = fromLists $ map (map digitToInt) rows

main :: IO ()
main = interact (unlines . process . lines)
