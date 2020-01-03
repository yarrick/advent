import qualified Data.Vector as V

type Recipes = (V.Vector Int,Int,Int)

results :: Int -> V.Vector Int
results n
  | n < 10 = V.singleton n
  | otherwise = V.cons 1 (V.singleton (mod n 10))

step :: Recipes -> Recipes
step (scores,cura,curb) = (scores V.++ (results newres), newpos cura, newpos curb)
  where newpos n = mod (n + 1 + (scores V.! n)) (length scores + length (results newres))
        newres = (scores V.! cura) + (scores V.! curb)

flow :: Int -> Int -> Recipes -> Recipes
flow n loglen rc@(rec,_,_)
  | length rec >= n + 10 = rc
  | otherwise = flow n loglen $ step rc

run n = concatMap show $ take 10 $ drop n $ V.toList vec
  where (vec,a,b) = flow n 1000 (V.cons 3 (V.singleton 7),0,1)
