import Data.List

type Xyz = (Int, Int, Int)
type Point = (Xyz, Xyz)
type Segment = ((Int,Int), (Int,Int))

cross :: (Segment, Segment) -> (Float, Float)
cross (((x1,y1),(x2,y2)), ((x3,y3),(x4,y4))) =
    (frac ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)), frac ((x1-x3)*(y1-y2) - (y1-y3)*(x1-x2)))
      where frac c = fdiv c ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4))

crossing :: (Int,Int) -> [(Segment,Segment)] -> [((Segment,Segment),(Float,Float))]
crossing _ [] = []
crossing (lo,hi) (s:ss)
    | t < 0.0 || u < 0.0 = crossing (lo,hi) ss
    | xc < fromIntegral lo || xc > fromIntegral hi = crossing (lo,hi) ss
    | yc < fromIntegral lo || yc > fromIntegral hi = crossing (lo,hi) ss
    | otherwise = (s,(xc,yc)): crossing (lo,hi) ss
    where (t,u) = cross s
          (xc,yc) = cpoint s t

cpoint :: (Segment,Segment) -> Float -> (Float,Float)
cpoint (((x1,y1),(x2,y2)),_) ratio = (p x1 x2, p y1 y2)
    where p st en = (fromIntegral st) + (fromIntegral (en - st))  * ratio

fdiv :: Int -> Int -> Float
fdiv a b = (fromIntegral a) / (fromIntegral b)

pairs :: [a] -> [(a,a)]
pairs (a:[]) = []
pairs (a:bs) = [(a,b) | b <- bs] ++ pairs bs

xylineseg :: (Int,Int) -> Point -> Segment
xylineseg (lo,hi) t@((x,y,z), (vx,vy,vz)) = ((x,y), (\((x,y,_),_) -> (x,y)) $ step 1 t)

step :: Int -> Point -> Point
step n ((x,y,z), (vx,vy,vz)) = ((x+n*vx,y+n*vy,z+n*vz), (vx,vy,vz))

process :: [Point] -> [String]
process ps = [ show $ length xycross ]
    where box
            | length ps < 10 = (7, 27) -- example
            | otherwise = (200000000000000, 400000000000000)
          xycross = crossing box $ pairs $ map (xylineseg box) ps

parse :: String -> Point
parse r = ((x,y,z), (ax,ay,az))
    where parts = map (filter (','/=)) $ words r
          (x:y:z:_) = map read parts
          (ax:ay:az:_) = map read $ drop 4 parts

main :: IO ()
main = interact (unlines . process . map parse . lines)
