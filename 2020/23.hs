import qualified Data.Sequence as S
import Data.Foldable

parse :: Int -> S.Seq Int
parse n = S.fromList $ map (\c -> read [c]) $ show n

cycleUntil :: S.Seq Int -> Int -> S.Seq Int
cycleUntil ss n
    | first == n = ss
    | otherwise = cycleUntil ((S.drop 1 ss) S.|> first) n
    where first = S.index ss 0

move :: S.Seq Int -> S.Seq Int
move ss = cycleUntil (S.take 1 cycled S.>< pickup S.>< S.drop 1 cycled) next
    where curr = S.index ss 0
          pickup = S.drop 1 $ S.take 4 ss
          remain = (S.drop 4 ss) S.>< (S.take 1 ss)
          next = S.index remain 0
          target 1 = target (S.length ss+1)
          target n
            | elem c pickup = target c
            | otherwise = c
            where c = pred n
          cycled = cycleUntil remain (target curr)

moves :: Int -> S.Seq Int -> S.Seq Int
moves 0 s = s
moves n s = moves (pred n) (move s)

result :: S.Seq Int -> String
result s = concatMap show $ tail $ toList $ cycleUntil s 1

run n = result $ moves 100 $ parse n
