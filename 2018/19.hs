import Device
import Data.Int (Int64)

divisors :: Int64 -> [Int64]
divisors k = 1 : divisors' 2 k
  where
    divisors' n k | n*n > k = [k]
                  | n*n == k = [n, k]
                  | k `mod` n == 0 = (n:(k `div` n):result)
                  | otherwise = result
      where result = divisors' (n+1) k

divisors2 :: Int64 -> [Int64]
divisors2 k = k : (concatMap (\x -> [x, k `div` x]) $
                   filter (\x -> k `mod` x == 0) $
                   takeWhile (\x -> x*x <= k) [2..])

-- Program calculates sum of divisors. Let it run for 500 cycles and
-- calculate the sum manually, using the target value from register 1.
solve :: State -> Int -> Int64
solve (pcreg,code,regs) reg0 = sum $ divisors $ fromIntegral $ endregs !! 1
  where (_,_,endregs) = execmax 500 (pcreg,code,reg0 : tail regs)

process :: State -> [String]
process st = map show [solve st 0, solve st 1]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
