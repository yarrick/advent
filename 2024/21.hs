process cs = walker (head cs)
    --[show $ sum $ map walked cs]
    where walk p c = steer (p, padpos p 'A') c
          walked (c,n) = n * (length $ walk dirpad $ walk dirpad $ walk numpad c)
          walker (c,n) = map show $ take 5 $ zip [0..] $ iterate (walk dirpad) np
            where np = walk numpad c

type Pos = (Int, Int) -- row, col
type PadSetup = [(Char, Pos)] -- position of each key
type PadState = (PadSetup, Pos)

numpad = [('7', (0,0)),   ('8', (0,1)),   ('9', (0,2)),
          ('4', (1,0)),   ('5', (1,1)),   ('6', (1,2)),
          ('1', (2,0)),   ('2', (2,1)),   ('3', (2,2)),
                          ('0', (3,1)),   ('A', (3,2))]

dirpad = [                ('^', (0,1)),   ('A', (0,2)),
          ('<', (1,0)),   ('v', (1,1)),   ('>', (1,2))]


padpos :: PadSetup -> Char -> Pos
padpos pad ch = snd $ head $ filter (\(p,_) -> p == ch) pad

steer :: PadState -> String -> String
steer _ [] = []
steer (pad, (sr,sc)) (d:ds)
    | dc > 0 && dr < 0 = move dc '>' ++ move dr '^' ++ "A" ++ steer nextpad ds
    | safedownright && dc > 0 = move dr 'v' ++ move dc '>' ++ "A" ++ steer nextpad ds
    | dc > 0 = move dc '>' ++ move dr 'v' ++ "A" ++ steer nextpad ds
    | dc == 0 && dr > 0 = move dr 'v' ++ "A" ++ steer nextpad ds
    | dc == 0 && dr < 0 = move dr '^' ++ "A" ++ steer nextpad ds
    | safeleftup && dc < 0 && dr < 0 = move dc '<' ++ move dr '^' ++ "A" ++ steer nextpad ds
    | dc < 0 && dr < 0 = move dr '^' ++ move dc '<' ++ "A" ++ steer nextpad ds
    | dc < 0 = move dr 'v' ++ move dc '<' ++ "A" ++ steer nextpad ds
    | otherwise = "A" ++ steer nextpad ds
    where (tr,tc) = padpos pad d
          dr = tr - sr
          dc = tc - sc
          move n c = take (abs n) (repeat c)
          (gaprow,_) = padpos pad 'A'
          safeleftup = gaprow == 0 || sr < 3 || tc > 0
          safedownright = sc > 0 || tr < 3
          nextpad = (pad, (tr,tc))

parse :: String -> (String, Int)
parse s = (s, read $ dropWhile ('0'==) $ filter ('A'/=) s)

main :: IO ()
main = interact (unlines . process . map parse . lines)
