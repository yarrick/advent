import qualified Data.Map as M

process cs = map show $ map res [2,25]
    where walk ds (c,n) = n * (solve (numpad: take ds (repeat dirpad)) c)
          res ds = sum $ map (walk ds) cs

type Pos = (Int, Int) -- row, col
type PadSetup = [(Char, Pos)] -- position of each key
type PadState = (PadSetup, Pos)
type Cache = M.Map ([PadState], Char) ([PadState], Int)

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

solve :: [PadState] -> String -> Int
solve ps ss = len
    where (_,len,c) = steer (ps,0,M.empty) ss

steer :: ([PadState], Int, Cache) -> String -> ([PadState], Int, Cache)
steer ([],v,c) a = ([], v + length a, c)
steer (a,v,c) [] = (a, v, c)
steer ((ps:pss),v,c) (d:ds)
    | M.member mkey c = steer (cps,v + clen, c) ds
    | otherwise = steer ((nps:pss),v + len, sc) ds
    where (nps, ss) = steerchar ps d
          mkey = ((ps:pss), d)
          (cps,clen) = c M.! mkey
          (npss, len, nc) = steer (pss,0,c) ss
          sc = M.insert mkey ((nps:pss), len) nc

steerchar :: PadState -> Char -> (PadState, String)
steerchar (pad, (sr, sc)) d = ((pad, (tr,tc)), keys ++ "A")
    where (tr,tc) = padpos pad d
          keys
            -- simple
            | dc == 0                           = downup
            | dr == 0                           = rightleft
            -- left and up
            | dc < 0 && dr < 0 && safeleftup    = rightleft ++ downup
            | dc < 0 && dr < 0                  = downup ++ rightleft
            -- left and down
            | dc < 0 && dr > 0 && safeleftdown  = rightleft ++ downup
            | dc < 0 && dr > 0                  = downup ++ rightleft
            -- right
            | dr < 0 && safeupright             = downup ++ rightleft
            | dr < 0                            = rightleft ++ downup
            | otherwise                         = downup ++ rightleft
            where dr = tr - sr
                  dc = tc - sc
                  downup = nmove dr "v^"
                  rightleft = nmove dc "><"
                  nmove n (a:b:[])
                    | n > 0 = take n (repeat a)
                    | otherwise = take (abs n) (repeat b)
                  (gaprow,_) = padpos pad 'A'
                  safeleftup = gaprow == 0 || sr < 3 || tc > 0
                  safeleftdown = tc > 0
                  safeupright = gaprow == 3 || sc > 0

parse :: String -> (String, Int)
parse s = (s, read $ dropWhile ('0'==) $ filter ('A'/=) s)

main :: IO ()
main = interact (unlines . process . map parse . lines)
