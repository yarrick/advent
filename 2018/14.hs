import qualified Data.Sequence as S
import Data.Foldable

type Recipes = (S.Seq Int,Int,Int)

startRec = (S.fromList [3,7], 0, 1)

append :: S.Seq Int -> Int -> S.Seq Int
append seq n
  | n < 10 = seq S.|> n
  | otherwise = (seq S.|> 1) S.|> (mod n 10)

step :: Recipes -> Recipes
step (scores,cura,curb) = (newscores, newpos cura, newpos curb)
  where newres = (S.index scores cura) + (S.index scores curb)
        newscores = append scores newres
        newpos n = mod (n + 1 + (S.index scores n)) (length newscores)

flow :: Int -> Recipes -> Recipes
flow n rc@(rec,_,_)
  | length rec >= n + 10 = rc
  | otherwise = flow n $ step rc

run :: Int -> String
run n = concatMap show $ take 10 $ drop n $ toList seq
  where (seq,a,b) = flow n startRec

flow2 :: [Int] -> Recipes -> S.Seq Int
flow2 wanted rc@(rec,_,_)
  | wanted == end = rec
  | wanted == take (length wanted) nend = S.deleteAt (S.length rec-1) rec
  | otherwise = flow2 wanted $ step rc
  where end = reverse $ toList $ S.take (length wanted) $ S.reverse rec
        nend = reverse $ toList $ S.take (length wanted + 1) $ S.reverse rec

run2 :: Int -> Int
run2 n = S.length seq - length wanted
  where wanted = [read [a] | a <- show n ]
        seq = flow2 wanted startRec

process :: [String] -> [String]
process (row:_) = [run num, show $ run2 num]
    where num = read row

main :: IO ()
main = interact (unlines . process . lines)
