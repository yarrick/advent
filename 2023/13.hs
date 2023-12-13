import Data.List
import qualified Data.Matrix as M
import qualified Data.Vector as V

process :: [M.Matrix Char] -> [String]
process mats = map show [head ans, last ans - head ans]
    where ans = map (\n -> (sum $ map (mirror n) mats)) [0,1]

mirror eps m = sum $ concat $ check (M.ncols,M.getCol,1) ++ check (M.nrows,M.getRow,100)
    where check (lenfn,getfn,x) = [ verify (lenfn,getfn) m n (n,n+1) x eps | n <- [1..lenfn m-1] ]

verify (lenfn,getfn) m lpos (left,right) x eps
    | left < 1 || right > lenfn m = [x*lpos]
    | diffs <= eps = verify (lenfn,getfn) m lpos (pred left,succ right) x (eps-diffs)
    | otherwise = []
    where c1 = V.toList $ getfn left m
          c2 = V.toList $ getfn right m
          diff prev (a,b)
            | a == b = prev
            | otherwise = succ prev
          diffs = foldl diff 0 $ zip c1 c2

parse :: [String] -> [M.Matrix Char]
parse [] = []
parse rows = M.fromLists mat : parse (drop 1 rest)
    where (mat,rest) = break (\r -> length r == 0) rows

main :: IO ()
main = interact (unlines . process . parse . lines)
