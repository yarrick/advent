import Data.Matrix
import Control.DeepSeq

parse :: String -> ((Int,Int),(Int,Int))
parse str = ((read posx, read velx), (read $ tail posy, read $ tail vely))
  where (pos, vel) = break ('>'==) $ take (length str-1) str
        (posx,posy) = break (','==) $ drop 10 pos
        (velx,vely) = break (','==) $ drop 12 vel

step :: [((Int,Int),(Int,Int))] -> [((Int,Int),(Int,Int))]
step state = deepseq nstate nstate
  where advance (pos,vel) = (pos+vel,vel)
        nstate = map (\(x,y) -> (advance x, advance y)) state

run :: Int -> [((Int,Int),(Int,Int))] -> ([((Int,Int),(Int,Int))], Int)
run iter state
  | maximum ypos - minimum ypos < 12 = (state,iter)
  | otherwise = run (iter+1) $ step state
  where ypos = map (fst.snd) state

process rows = [ show $ foldl (\m (r,c) -> setElem 7 (r+3-miny,c+3-minx) m) zerom pos,
                 show timer ]
  where (result,timer) = run 0 $ map parse rows
        pos = map (\((px,_),(py,_)) -> (py,px)) result
        ypos = map fst pos
        xpos = map snd pos
        miny = minimum ypos
        minx = minimum ypos
        zerom = zero (maximum ypos - minimum ypos + 6) (maximum xpos - minimum xpos + 6)

main :: IO ()
main = interact (unlines . process . lines)
