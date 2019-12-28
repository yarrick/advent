import Data.List

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

process :: [String] -> [String]
process rows = [show $ shuffle (map (parse.words) rows) 10007 2019]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
