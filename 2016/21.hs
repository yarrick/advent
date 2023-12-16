import Data.List
import Data.Maybe

data Action =
  SwapPosition Int Int |
  SwapLetter Char Char |
  RotateWhole String Int |
  RotateCharIndex Char |
  ReversePos Int Int |
  MovePosition Int Int deriving (Show)

firstPos :: String -> Char -> Int
firstPos str c = fromJust $ findIndex (c==) str

parse :: [String] -> [Action]
parse [] = []
parse ("swap":"position":x:_:_:y:ps) = SwapPosition (read x) (read y) : parse ps
parse ("swap":"letter":x:_:_:y:ps) = SwapLetter (head x) (head y) : parse ps
parse ("rotate":"based":_:_:_:_:x:ps) = RotateCharIndex (head x) : parse ps
parse ("rotate":dir:x:_:ps) = RotateWhole dir (read x) : parse ps
parse ("reverse":_:x:_:y:ps) = ReversePos (read x) (read y) : parse ps
parse ("move":_:x:_:_:y:ps) = MovePosition (read x) (read y) : parse ps

apply :: String -> Action -> String
apply a (SwapPosition x y)
  | x < y = (take x a) ++ [a !! y] ++ (drop (x+1) $ take y a) ++ [a !! x] ++ (drop (y+1) a)
  | otherwise = apply a (SwapPosition y x)
apply a (SwapLetter c d) = apply a (SwapPosition (firstPos a c) (firstPos a d))
apply a (RotateWhole "left" x) = drop r a ++ take r a
  where r = mod x (length a)
apply a (RotateWhole "right" x) = drop r a ++ take r a
  where r = length a - mod x (length a)
apply a (RotateCharIndex c)
  | cp >= 4 = apply a (RotateWhole "right" $ 2 + cp)
  | otherwise = apply a (RotateWhole "right" $ 1 + cp)
  where cp = firstPos a c
apply a (ReversePos x y) = (take x a) ++ reverse (drop x $ take (y+1) a) ++ drop (y+1) a
apply a (MovePosition x y) = take y removed ++ [a !! x] ++ drop y removed
  where removed = take x a ++ drop (x+1) a

part1 :: [String] -> String
part1 rows = foldl apply "abcdefgh" $ parse $ concat $ map words rows

revapply :: String -> Action -> String
revapply a (SwapPosition x y) = apply a (SwapPosition x y)
revapply a (SwapLetter c d) = apply a (SwapLetter c d)
revapply a (RotateWhole "left" x) = apply a (RotateWhole "right" x)
revapply a (RotateWhole "right" x) = apply a (RotateWhole "left" x)
revapply a (RotateCharIndex c) = snd $ head $ filter (\(r,s) -> firstPos s c == r) options
  where
    pos r
      | r > 5 = r - 2
      | otherwise = r - 1
    options = [ (pos rots, apply str (RotateWhole "left" rots)) |
                (rots, str) <- zip [1..(length a + 2)] (repeat a)]
revapply a (ReversePos x y) = apply a (ReversePos x y)
revapply a (MovePosition x y) = apply a (MovePosition y x)

part2 :: [String] -> String
part2 rows = foldl revapply "fbgdceah" $ parse $ concat $ map words $ reverse rows

process :: [String] -> [String]
process rows = [part1 rows, part2 rows]

main :: IO ()
main = interact (unlines . process . lines)
