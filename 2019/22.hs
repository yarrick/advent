import Data.List

dealnew :: [Int] -> [Int]
dealnew = reverse

cut :: Int -> [Int] -> [Int]
cut pos cards
  | pos >= 0 = drop pos cards ++ take pos cards
  | otherwise = cut (length cards - (abs pos)) cards

dealinc :: Int -> [Int] -> [Int]
dealinc n cards = map snd $ sortBy (\(a,_) (b,_) -> compare a b) cardorder
   where cardorder = map (\(a,b) -> (mod (n*a) (length cards),b)) $ zip [0..] cards

parse :: [String] -> ([Int] -> [Int])
parse ("deal":"with":_:num:rest) = dealinc $ read num
parse ("deal":"into":rest) = dealnew
parse ("cut":num:rest) = cut $ read num

part1 [] cards = cards
part1 (m:ms) cards = part1 ms (m cards)

process :: [String] -> [String]
process rows = [show $ findIndex (2019==) $ part1 moves [0..10006]]
  where moves = map (parse.words) rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
