import Data.List
import Data.Maybe
import qualified Data.Map as M

data Rule = North | South | West | East deriving (Eq, Show)
type State = M.Map (Int,Int) [Rule]

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x-1,y-1), (x,y-1), (x+1,y-1),
                   (x-1,y),            (x+1,y),
                   (x-1,y+1), (x,y+1), (x+1,y+1)]

canmove :: State -> (Int,Int) ->
            ((Int,Int) -> Bool, (Int,Int)) -> Maybe (Int,Int)
canmove m (x,y) (func, npos)
    | length nbors == 0 = Just npos
    | otherwise = Nothing
    where nbors = filter func $ filter (\p -> M.member p m) $ neighbors (x,y)

checker :: Rule -> (Int,Int) -> ((Int,Int) -> Bool, (Int,Int))
checker North (x,y) = ((\(nx,ny) -> ny < y), (x,y-1))
checker South (x,y) = ((\(nx,ny) -> ny > y), (x,y+1))
checker West (x,y) = ((\(nx,ny) -> nx < x), (x-1,y))
checker East (x,y) = ((\(nx,ny) -> nx > x), (x+1,y))

plan :: State -> (Int,Int) -> ((Int,Int), (Int,Int), [Rule])
plan m pos
    | length nbors == 0 || length passed == 0 = (pos, pos, nrules)
    | otherwise = (fromJust $ head passed, pos, nrules)
    where nbors = filter (\p -> M.member p m) $ neighbors pos
          rules = (M.!) m pos
          passed = filter isJust $ map (\r -> canmove m pos (checker r pos)) rules
          nrules = tail rules ++ [head rules]

step m = M.fromList $ map move plans
    where plans = map (plan m) $ M.keys m
          dups = map head $ filter ((>1).length) $ group $ sort $ map (\(np,_,_) -> np) plans
          move (np,p,rm)
            | elem np dups = (p,rm)
            | otherwise = (np,rm)

empties :: State -> Int
empties m = width * height - M.size m
    where (xs,ys) = unzip $ M.keys m
          width = maximum xs - minimum xs + 1
          height = maximum ys - minimum ys + 1

dup (a:b:cs)
    | M.keys a == M.keys b = [1]
    | otherwise = 1 : dup (b:cs)

process rows = map show [empties (moves !! 10), length $ dup moves]
    where m = M.fromList $ zip rows $ cycle [[North,South,West,East]]
          moves = iterate step m

parse :: [String] -> [(Int,Int)]
parse rows = concatMap tagy $ zip [1..] $ map tagx rows
    where tagx r = map fst $ filter ((=='#').snd) $ zip [1..] r
          tagy (y,xs) = map (\x -> (x,y)) xs

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

