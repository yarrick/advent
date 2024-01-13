import Data.List
import Data.Maybe
import Data.Ratio

type Segment = ((Int,Int), (Int,Int))

xylineseg ((x,vx):(y,vy):_) = ((x,y), (x+vx,y+vy))
xzlineseg ((x,vx):_:(y,vy):_) = ((x,y), (x+vx,y+vy))

cross :: (Segment, Segment) -> Maybe [Ratio Int]
cross (((x1,y1),(x2,y2)), ((x3,y3),(x4,y4)))
    | denom == 0 = Nothing
    | otherwise = Just [((x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) % denom,
                        ((x1-x3)*(y1-y2) - (y1-y3)*(x1-x2)) % denom]
      where denom = ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4))

crossing :: [Int] -> (Segment, Segment) -> Bool
crossing box s
    | isNothing tu = False
    | otherwise = t >= 0 && u >= 0 && minimum xy >= lo && maximum xy <= hi
    where tu = cross s
          [t,u] = fromJust tu
          xy = cpoint s t
          [lo,hi] = map (\n -> n % 1) box

cpoint :: (Segment, Segment) -> Ratio Int -> [Ratio Int]
cpoint (((x1,y1),(x2,y2)),_) ratio = [p x1 x2, p y1 y2]
    where p st en = (fromIntegral st) + (fromIntegral (en - st)) * ratio

hitting :: [(Int,Int)] -> [[(Int,Int)]] -> Maybe [(Int,Int)]
hitting rock hails
    | all (\h -> length h == 1 && head h > 0) hits = Just rock
    | otherwise = Nothing
    where hits = [ nub $ concat $ catMaybes $ map (\fn -> cross (fn rock, fn h)) [xylineseg, xzlineseg] | h <- hails]

startpos :: [(Int,Int)] -> [Int] -> Ratio Int -> [Int]
startpos hail aim t1
    | all (\r -> denominator r == 1) start = map numerator start
    | otherwise = []
    where start = map pos $ zip hail aim
          pos ((p0,pv),rv) = fromIntegral p0 + t1 * fromIntegral (pv - rv)

-- Based on differences between positions of two hails at t1 and t2
-- where vx,vy is the direction of the thrown stone.
thrown :: [[(Int,Int)]] -> (Int,Int) -> Maybe [(Int,Int)]
thrown (h1@[(ax,avx),(ay,vy1),(az,avz)]:[(bx,bvx),(by,bvy),(bz,bvz)]:ps) (vx,vy)
    | denom == 0 || t1 < 0 || t2 < 0 = Nothing
    | denominator vz_ratio /= 1 = Nothing
    | initial == [] = Nothing
    | otherwise = hitting (zip initial aim) ps
    where denom =  vx*vy1 - vx*bvy - avx*vy + avx*bvy + bvx*vy - bvx*vy1
          t1 = (-vx*ay + vx*by + bvx*ay - bvx*by + vy*ax - vy*bx - bvy*ax + bvy*bx) % denom
          t2 = (-vx*ay + vx*by + avx*ay - avx*by + vy*ax - vy*bx - vy1*ax + vy1*bx) % denom
          vz_ratio = ((fromIntegral az + t1*fromIntegral avz) - (fromIntegral bz + t2*fromIntegral bvz)) / (t1 - t2)
          aim = [vx,vy,numerator vz_ratio]
          initial = startpos h1 aim t1

outer :: Int -> [(Int,Int)]
outer n  = horiz ++ vert ++ outer (succ n)
    where horiz = [ (x,y) | x <- [-n..n], y <- [-n,n] ]
          vert = [ (x,y) | x <- [-n,n], y <- [(1-n)..(n-1)] ]

process :: [[(Int, Int)]] -> [String]
process ps = map show [ length $ filter (crossing box) $ pairs $ map xylineseg ps, sum $ map fst rock]
    where box
            | length ps < 10 = [7, 27] -- For example data
            | otherwise = [200000000000000, 400000000000000]
          pairs [a] = []
          pairs (a:bs) = [(a,b) | b <- bs] ++ pairs bs
          vxys = (0,0) : outer 1
          rock = head $ catMaybes $ map (thrown ps) vxys

parse :: String -> [(Int, Int)]
parse r = [(x,ax),(y,ay),(z,az)]
    where (x:y:z:_:ax:ay:az:_) = map (read.filter (','/=)) $ words r

main :: IO ()
main = interact (unlines . process . map parse . lines)
