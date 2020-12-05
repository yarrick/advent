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

divsum (p,k) = sum $ 1 : map (\x -> p^x) [1..k]

gifts n = 10 * (product $ map divsum $ pfactor n)

firsthouse n = head $ filter (\(_,v) -> v >= n) houses
    where houses = zip [1..] (map gifts [1..])

main = print $ show $ firsthouse 29000000
