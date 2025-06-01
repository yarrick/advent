-- 257185799027468 too high (214 minutes)
process cs = map show $ map res [2,20]
    where walk ds (c,n) = n * (steer (numpad: take ds (repeat dirpad)) c)
          res ds = sum $ map (walk ds) cs

type Pos = (Int, Int) -- row, col
type PadSetup = [(Char, Pos)] -- position of each key
type PadState = (PadSetup, Pos)

numpad = ([('7', (0,0)),   ('8', (0,1)),   ('9', (0,2)),
           ('4', (1,0)),   ('5', (1,1)),   ('6', (1,2)),
           ('1', (2,0)),   ('2', (2,1)),   ('3', (2,2)),
                           ('0', (3,1)),   ('A', (3,2))],
          (3,2))

dirpad = ([                ('^', (0,1)),   ('A', (0,2)),
           ('<', (1,0)),   ('v', (1,1)),   ('>', (1,2))],
          (0,2))


padpos :: PadSetup -> Char -> Pos
padpos pad ch = snd $ head $ filter (\(p,_) -> p == ch) pad

steer :: [PadState] -> String -> Int
steer [] a = length a
steer _ [] = 0
steer (ps:pss) (d:ds) = steer pss ss + steer (nps:pss) ds
    where (nps, ss) = steerchar ps d

steerchar :: PadState -> Char -> (PadState, String)
steerchar (pad, (sr, sc)) d = ((pad, (tr,tc)), keys ++ "A")
    where (tr,tc) = padpos pad d
          keys
            | dc > 0 && dr < 0 = move dc '>' ++ move dr '^'
            | safedownright && dc > 0 = move dr 'v' ++ move dc '>'
            | dc > 0 = move dc '>' ++ move dr 'v'
            | dc == 0 && dr > 0 = move dr 'v'
            | dc == 0 && dr < 0 = move dr '^'
            | safeleftup && dc < 0 && dr < 0 = move dc '<' ++ move dr '^'
            | dc < 0 && dr < 0 = move dr '^' ++ move dc '<'
            | dc < 0 = move dr 'v' ++ move dc '<'
            | otherwise = []
            where dr = tr - sr
                  dc = tc - sc
                  move n c = take (abs n) (repeat c)
                  (gaprow,_) = padpos pad 'A'
                  safeleftup = gaprow == 0 || sr < 3 || tc > 0
                  safedownright = sc > 0 || tr < 3

parse :: String -> (String, Int)
parse s = (s, read $ dropWhile ('0'==) $ filter ('A'/=) s)

main :: IO ()
main = interact (unlines . process . map parse . lines)
