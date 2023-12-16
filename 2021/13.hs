import Data.List
import Data.Matrix

fold :: (Char,Int) -> (Int,Int) -> (Int,Int)
fold (ax,pos) (x,y)
    | ax == 'x' && x < pos = (x,y)
    | ax == 'x' = (2*pos - x, y)
    | ax == 'y' && y < pos = (x,y)
    | otherwise = (x, 2*pos - y)

draw :: [(Int,Int)] -> Matrix Char
draw ps = matrix (dim snd) (dim fst) point
    where dim fn = succ $ maximum $ map fn ps
          point (r,c)
            | elem (c-1,r-1) ps = '#'
            | otherwise = ' '

process :: ([(Int,Int)], [(Char,Int)]) -> [String]
process (points,folds) = [show firstfold] ++ toLists (draw folded)
    where foldall ps f = map (fold f) ps
          firstfold = length $ nub $ foldall points (head folds)
          folded = nub $ foldl foldall points folds

parse :: [String] -> ([(Int,Int)], [(Char,Int)])
parse rows = (map point ps, map (fold.words) $ tail folds)
    where (ps, folds) = break (""==) rows
          fold ("fold":"along":a:_) = (head a, read $ drop 2 a)
          point r = read $ "(" ++ r ++ ")"

main :: IO ()
main = interact (unlines . process . parse . lines)
