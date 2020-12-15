import qualified Data.Sequence as S

store :: S.Seq (Int,Int) -> (Int,Int) -> S.Seq (Int,Int)
store hist (val,round) = S.update val (prev,round) hist
    where (pp,prev) = S.index hist val

startHist :: S.Seq (Int,Int)
startHist = S.replicate 10000 (-1,-1)

next :: S.Seq (Int,Int) -> Int -> Int -> (Int, S.Seq (Int,Int))
next hist val round
    | pprev < 0 = (0, store hist (0,round))
    | otherwise = (rdiff, store hist (rdiff,round))
    where (pprev,prev) = S.index hist val
          rdiff = prev - pprev

step :: S.Seq (Int,Int) -> Int -> Int -> [Int]
step hist val round = nval : step nhist nval (succ round)
    where (nval, nhist) = next hist val round

run :: [Int] -> [Int]
run starts = [stream !! 2019]
    where hist = foldl store startHist $ zip starts [1..]
          stream = starts ++ step hist (last starts) (length starts + 1)
