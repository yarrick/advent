import qualified Data.Sequence as S

type Ring = (S.Seq Int, Int)

step :: Int -> Ring -> Int -> Ring
step gap (r,p) n = (S.insertAt pos n r, pos)
    where pos = 1 + mod (p+gap) (S.length r)

run :: Int -> Int
run n = S.index end (pos+1)
    where r = (S.singleton 0, 0)
          (end,pos) = foldl (step n) r [1..2017]
