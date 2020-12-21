import Data.List

type Ingredients = ([String],[String])

parse :: String -> Ingredients
parse row = (sort $ words stuffs, tail $ words allergens)
    where stuffs = takeWhile (/='(') row
          end = filter (/=',') $ drop (length stuffs) row
          allergens = take (length end-1) end

shared :: [[String]] -> [String]
shared foods
    | elem [] foods = []
    | length foods == 1 = head foods
    | length (nub is) == 1 = maxi : shared (map tail foods)
    | otherwise = shared $ map step foods
    where is = map head foods
          maxi = maximum is
          step food
            | head food == maxi = food
            | otherwise = tail food

exclude :: (String,String) -> [Ingredients] -> [Ingredients]
exclude _ [] = []
exclude (food, allg) ((fs,as):is) = (filter (/=food) fs, filter (/=allg) as) : exclude (food,allg) is

decide :: ([(String,String)],[Ingredients]) -> String -> ([(String,String)], [Ingredients])
decide (known,rows) alg
    | length common == 1 = (res:known, exclude res rows)
    | otherwise = (known, rows)
    where cands = map (sort.fst) $ filter (\(a,b) -> elem alg b) rows
          common = shared cands
          res = (head common, alg)

deduce :: ([(String,String)], [Ingredients]) -> ([(String,String)], [Ingredients])
deduce (known, foods)
    | known == res && foods == rest = (known,foods)
    | otherwise = deduce (res,rest)
    where unknown = map snd $ sort $ map (\g -> (1000 - length g, head g)) $ group $ sort $ concatMap snd foods
          (res,rest) = foldl decide (known,foods) unknown

process :: [Ingredients] -> [String]
process rows = [show $ length $ concat $ map fst leftover,
                concat $ intersperse "," $ map fst $ sortBy (\(_,a) (_,b) -> compare a b) assigned]
    where (assigned, leftover) = deduce ([], rows)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map parse) . lines)

