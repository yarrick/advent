import Asmbunny

run str = fst $ head $ filter keeper vms
  where vms = map (\a -> (head a, runVM $ newVM (readInstr $ words str) a)) starts
        starts = map (\x -> x:0:0:0:[]) [0..]

keeper :: (Int,VM) -> Bool
keeper (i,(_,_,_,o)) = onezero o

onezero :: [Int] -> Bool
onezero [] = True
onezero (0:1:xs) = onezero xs
onezero _ = False
