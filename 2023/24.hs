type Segment = ((Int,Int), (Int,Int))

cross :: (Segment, Segment) -> (Float, Float)
cross (((x1,y1),(x2,y2)), ((x3,y3),(x4,y4))) =
    (frac ((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)), frac ((x1-x3)*(y1-y2) - (y1-y3)*(x1-x2)))
      where frac c = (fromIntegral c) / (fromIntegral ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)))

crossing :: (Float, Float) -> (Segment, Segment) -> Bool
crossing (lo,hi) s = t >= 0.0 && u >= 0.0 && minimum xy >= lo && maximum xy <= hi
    where (t,u) = cross s
          xy = cpoint s t

cpoint :: (Segment, Segment) -> Float -> [Float]
cpoint (((x1,y1),(x2,y2)),_) ratio = [p x1 x2, p y1 y2]
    where p st en = (fromIntegral st) + (fromIntegral (en - st)) * ratio

process :: [[(Int, Int)]] -> [String]
process ps = [ show $ length $ filter (crossing box) $ pairs $ map xylineseg ps ]
    where box
            | length ps < 10 = (7.0, 27.0) -- For example data
            | otherwise = (200000000000000.0, 400000000000000.0)
          xylineseg ((x,vx):(y,vy):_) = ((x,y), (x+vx,y+vy))
          pairs [a] = []
          pairs (a:bs) = [(a,b) | b <- bs] ++ pairs bs

parse :: String -> [(Int, Int)]
parse r = [(x,ax),(y,ay),(z,az)]
    where (x:y:z:_:ax:ay:az:_) = map (read.filter (','/=)) $ words r

main :: IO ()
main = interact (unlines . process . map parse . lines)
