import Data.List
import Data.Foldable (foldl')
import Math.NumberTheory.Moduli (SomeMod(..), modulo, getVal)

dealnew :: Integer -> Integer -> Integer
dealnew decksize card = decksize - card - 1

dealinc :: Integer -> Integer -> Integer -> Integer
dealinc n decksize card = mod (n*card) decksize

cut :: Integer -> Integer -> Integer -> Integer
cut pos decksize card
  | pos >= 0 && card < pos = decksize - pos + card
  | pos >= 0 = card - pos
  | otherwise = cut (decksize - (abs pos)) decksize card

parse :: [String] -> (Integer -> Integer -> Integer)
parse ("deal":"with":_:num:rest) = dealinc $ read num
parse ("deal":"into":rest) = dealnew
parse ("cut":num:rest) = cut $ read num

shuffle :: [(Integer -> Integer -> Integer)] -> Integer -> Integer -> Integer
shuffle [] _ pos = pos
shuffle (m:ms) decksize pos = shuffle ms decksize (m decksize pos)

-- part 2
-- I don't really understand this math.

data Op = Cut SomeMod | DealInc SomeMod | DealNew deriving (Eq,Show)

part2 :: [String] -> SomeMod
part2 str = base + 2020 * inc
  where n = 119315717514047
        loops = 101741582076661
        moves = getop . words <$> str
        getop ("deal":"with":_:num:rest) = DealInc (read num `modulo` n)
        getop ("deal":"into":rest) = DealNew
        getop ("cut":num:rest) = Cut (read num `modulo` n)
        (b,i) = foldl' (apply n) (0 `modulo` n, 1 `modulo` n) moves
        apply n (b, i) (Cut c) = (b + c * i, i)
        apply n (b, i) (DealInc c) = (b, i / c)
        apply n (b, i) (DealNew) = (b - i, -i)
        base = b * (1 - i^loops) / (1 - i)
        inc = i ^ loops

process :: [String] -> [String]
process rows =
 [show $ shuffle (map (parse.words) rows) 10007 2019,
  show $ part2 rows]

main :: IO ()
main = interact (unlines . process . lines)
