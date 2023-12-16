import Data.List

type Xyz = (Int, Int, Int)
type Point = (Int, Xyz, Xyz, Xyz)

parse :: (Int, String) -> Point
parse (num,r) = (num, pos, vec, acc)
    where prep (_:'=':'<':ps) = '(' : prep ps
          prep ('>':ps) = ")"
          prep (p:ps) = p : prep ps
          (pos:vec:acc:[]) = map (read.prep) $ words r

step :: Point -> Point
step (num, (x,y,z), (vx,vy,vz), acc@(ax,ay,az)) =
    (num, (x+vx+ax,y+vy+ay,z+vz+az), (vx+ax,vy+ay,vz+az), acc)

collide :: [Point] -> [Point]
collide = concat . filter (\g -> length g == 1) . groupBy pos
    where pos (_,pa,_,_) (_,pb,_,_) = pa == pb

process :: [String] -> [String]
process rows = map show [ snd $ head $ sort $ map dist moved1, length moved2 ]
    where points = map parse $ zip [0..] rows
          moved1 = (iterate (map step) points) !! 1000
          moved2 = (iterate (collide . map step) points) !! 1000
          dist (num,(x,y,z),_,_) = (sum $ map abs [x,y,z], num)

main :: IO ()
main = interact (unlines . process . lines)
