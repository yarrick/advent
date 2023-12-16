import Data.Char
import Data.List

type Pos = (Int, Int)

move :: Pos -> Char -> Pos
move (x,y) 'R' = (x+1,y)
move (x,y) 'L' = (x-1,y)
move (x,y) 'U' = (x,y+1)
move (x,y) 'D' = (x,y-1)

follow :: Pos -> Pos -> Pos
follow (hx,hy) (tx,ty)
    | ty == hy && hx == tx = (tx,ty)
    | ty == hy && abs (hx - tx) == 1 = (tx,ty)
    | ty == hy && hx - tx > 1 = (tx+(hx-tx)-1,ty)
    | ty == hy && hx - tx < -1 = (tx+(hx-tx)+1,ty)
    | tx == hx && abs (hy - ty) == 1 = (tx,ty)
    | tx == hx && hy - ty > 1 = (tx,ty+(hy-ty)-1)
    | tx == hx && hy - ty < -1 = (tx,ty+(hy-ty)+1)
    | abs (hx - tx) == 1 && abs (hy - ty) == 1 = (tx,ty)
    | abs (hx - tx) == 1 && hy - ty > 1 = (hx,ty+(hy-ty)-1)
    | abs (hx - tx) == 1 && hy - ty < -1 = (hx,ty+(hy-ty)+1)
    | abs (hy - ty) == 1 && hx - tx > 1 = (tx+(hx-tx)-1,hy)
    | abs (hy - ty) == 1 && hx - tx < -1 = (tx+(hx-tx)+1,hy)
    | hx - tx == 2 && hy - ty == 2 = (tx+1,ty+1)
    | hx - tx == 2 && hy - ty == -2 = (tx+1,ty-1)
    | hx - tx == -2 && hy - ty == 2 = (tx-1,ty+1)
    | hx - tx == -2 && hy - ty == -2 = (tx-1,ty-1)

walk :: [(Pos, Pos)] -> (Char, Int) -> [(Pos, Pos)]
walk p (_,0) = p
walk ((h,t):ps) (dir,len) = walk ((nexth, follow nexth t):(h,t):ps) (dir, pred len)
    where nexth = move h dir

walk2 :: [[Pos]] -> (Char, Int) -> [[Pos]]
walk2 p (_,0) = p
walk2 ((h:tt):ps) (dir,len) = walk2 ((scanl follow nexth tt):(h:tt):ps) (dir, pred len)
    where nexth = move h dir

process :: [(Char, Int)] -> [String]
process s = map (show.length.nub) [map snd trail1, map last trail2]
    where start = (0,0)
          trail1 = foldl walk [(start,start)] s
          trail2 = foldl walk2 [replicate 10 start] s

parse :: String -> (Char, Int)
parse r = (head r, read $ dropWhile (not . isDigit) r)

main :: IO ()
main = interact (unlines . process . map parse . lines)
