import qualified Data.Sequence as S
import Control.DeepSeq

store :: S.Seq (Int,Int) -> (Int,Int) -> S.Seq (Int,Int)
store hist (val,round) = S.update val (prev,round) hist
    where (pp,prev) = S.index hist val

startHist :: S.Seq (Int,Int)
startHist = S.replicate 99999999 (-1,-1)

next :: S.Seq (Int,Int) -> Int -> Int -> (Int, S.Seq (Int,Int))
next hist val round
    | pprev < 0 = (0, store hist (0,round))
    | otherwise = (rdiff, store hist (rdiff,round))
    where (pprev,prev) = S.index hist val
          rdiff = prev - pprev

step :: S.Seq (Int,Int) -> Int -> Int -> Int -> Int
step hist val goal round
    | goal == round = nval
    | mod round 5000000 == 0 = deepseq n (step nhist nval goal (succ round))
    | otherwise = step nhist nval goal (succ round)
    where n = next hist val round
          (nval, nhist) = seq n n

run :: [Int] -> Int -> Int
run starts n = step hist (last starts) n (length starts+1)
    where hist = foldl store startHist $ zip starts [1..]

process :: [String] -> [String]
process (row:_) = map show [run nums 2020, run nums 30000000]
    where nums = read $ "[" ++ row ++ "]"

main :: IO ()
main = interact (unlines . process . lines)
