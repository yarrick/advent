import Data.Bits

process m = [tail $ init $ show $ step m, show $ copier m]

copier (pc, regs, code)
    | end /= [3, 0] || not (div8 code) = 0
    | otherwise = quine (pc, regs, code) 0 (length code - 1)
    where end = drop ((length code) - 2) code
          div8 [] = False
          div8 (0:3:cs) = True
          div8 (_:_:cs) = div8 cs

quine (pc, regs, code) val drops
    | drops == 0 = min
    | otherwise = quine (pc, regs, code) min (pred drops)
    where range = [(x+val)*8+y | x <- [0..7], y <- [0..7]]
          ends = map (\n -> (n, step (pc, n : tail regs, code))) range
          min = fst $ head $ filter (\(_,e) -> e == drop drops code) ends

combo :: [Int] -> Int -> Int
combo regs c
    | c <= 3 = c
    | c <= 6 = regs !! (c-4)

diva regs c = div (head regs) $ product $ take (combo regs c) $ cycle [2]
bxorlit regs l = xor (regs !! 1) l
bxorc regs _ = xor (regs !! 1) (regs !! 2)
modulo regs c = mod (combo regs c) 8
jnz regs l
    | head regs == 0 = -1
    | otherwise = l

ops = [ -- (func, dest register [when >= 0]).
    (diva, 0),
    (bxorlit, 1),
    (modulo, 1),
    (jnz, -1),
    (bxorc, 1),
    (modulo, -1),
    (diva, 1),
    (diva, 2)]

step :: (Int, [Int], [Int]) -> [Int]
step (pc, regs, code)
    | pc >= length code = []
    | ins == 3 && res >= 0 = step (res, regs, code) -- jnz
    | ins == 5 = res : step (pc+2, regs, code) -- out
    | otherwise = step (pc+2, newregs, code)
    where (ins:oper:_) = take 2 $ drop pc code
          (fn, dest) = ops !! ins
          res = fn regs oper
          newregs = take dest regs ++ [res] ++ drop (dest+1) regs

parse :: [String] -> (Int, [Int], [Int])
parse ss = (0, map (read.last.words) regs, read $ "[" ++ drop 9 ops ++ "]")
    where (regs,(blank:ops:[])) = break (""==) ss

main :: IO ()
main = interact (unlines . process . parse . lines)
