import Control.Parallel.Strategies

factors :: Int -> [Int]
factors n = [x | x <- [1..(div n 2)], mod n x == 0] ++ [n]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

primelist = primes 50000

pdivs n p res
    | m == 0 = pdivs d p (res+1)
    | otherwise = (res, n)
    where (d,m) = divMod n p

pfact n [] = []
pfact n (p:rimes)
    | p > n = []
    | divs > 0 = (p,divs) : pfact rest rimes
    | otherwise = pfact n rimes
    where (divs, rest) = pdivs n p 0

pfactor n = pfact n primelist

allproducts [] = []
allproducts [a] = a
allproducts (a:b:cs) = allproducts (n:cs)
    where n = [x*y | x <- a, y <- b]

exps (p,k) = map (\x -> p^x) [0..k]

allfactors n = allproducts $ map exps $ pfactor n

gifts n = 10 * (sum $ allfactors n)

gifts2 n = 11 * (sum $ filter active $ allfactors n)
    where active x = div n x <= 50

firsthouse n = head $ filter (\(_,v) -> v >= n) houses
    where houses = zip [1..] (map gifts [1..])

firsthouse2 n = head $ filter (\(_,v) -> v >= n) houses
    where houses = zip [1..] (map gifts2 [1..])

main = mapM print $ parMap rpar (\op -> show $ fst $ op 29000000) [firsthouse, firsthouse2]

-- ghc -threaded --make 20.hs
-- time ./20 +RTS -N2
