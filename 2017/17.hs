import qualified Data.Sequence as S
import Data.Foldable

type Ring = (S.Seq Int, Int)

step :: Int -> Ring -> Int -> Ring
step gap (r,p) n = (S.insertAt pos n r, pos)
    where pos = 1 + mod (p+gap) (S.length r)

run :: Int -> Int
run n = S.index end (pos+1)
    where r = (S.singleton 0, 0)
          (end,pos) = foldl' (step n) r [1..2017]

-- value after 0, total length, pos
type FastRing = (Int, Int, Int)

fstep :: Int -> FastRing -> Int -> FastRing
fstep gap (zv,len,p) n
    | npos == 1 = (n, succ len, npos)
    | otherwise = (zv, succ len, npos)
    where npos = 1 + mod (p+gap) len

run2 :: Int -> Int
run2 n = zv
    where (zv,_,_) = foldl' (fstep n) (0,1,0) [1..50000000]

process :: [String] -> [String]
process (row:_) = map show [run num, run2 num]
    where num = read row

main :: IO ()
main = interact (unlines . process . lines)
