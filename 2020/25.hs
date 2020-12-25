
loop :: Int -> Int -> Int -> [(Int,Int)]
loop val sub n = (n,val) : loop next sub (succ n)
    where next = mod (val*sub) 20201227

crack :: [Int] -> [(Int,Int)] -> (Int,Int)
crack vals ((n,key):ks)
    | elem key vals = (n,key)
    | otherwise = crack vals ks

run :: Int -> Int -> Int
run a b
    | key == a = res b
    | otherwise = res a
    where (n,key) = crack [a,b] (loop 1 7 0)
          res k = snd $ (loop 1 k n) !! n
