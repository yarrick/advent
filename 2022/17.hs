import Data.List
import qualified Data.Map as M

tileset =
    [   -- (x,y)
        [(0,0), (1,0), (2,0), (3,0)],
        [(1,2), (0,1), (1,1), (2,1), (1,0)],
        [(2,2), (2,1), (0,0), (1,0), (2,0)],
        [(0,3), (0,2), (0,1), (0,0)],
        [(0,1), (1,1), (0,0), (1,0)]
    ]

expand :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
expand (x,y) tile = map (\(a,b) -> (x+a, y+b)) tile

overlap :: M.Map (Int,Int) Bool -> (Int, Int) -> [(Int,Int)] -> Bool
overlap mp pos tile = (any id $ map (\p -> M.member p mp) spots) || (any (\(x,y) -> y == 0) spots)
    where spots = expand pos tile

fall :: (M.Map (Int,Int) Bool, Int, String) -> (Int, Int) -> [(Int,Int)] -> (M.Map (Int,Int) Bool, Int, String)
fall (mp, height, (m:moves)) (x,y) tile
    | overlap mp fallen tile = (foldl mark mp (expand blown tile), newheight blown, moves)
    | otherwise = fall (mp, height, moves) fallen tile
    where width = maximum (map fst tile)
          blown
            | m == '<' && x == 1 = (x,y)
            | m == '<' && overlap mp (x-1,y) tile = (x,y)
            | m == '<' = (x-1,y)
            | x + width == 7 = (x,y)
            | overlap mp (x+1,y) tile = (x,y)
            | otherwise = (x+1,y)
          fallen = (fst blown, pred $ snd blown)
          mark mpp p = M.insert p True mpp
          newheight p = maximum $ height : (map snd $ expand p tile)

play :: (M.Map (Int,Int) Bool, Int, String) -> [(Int,Int)] -> (M.Map (Int,Int) Bool, Int, String)
play (mp, height, moves) tile = fall (mp, height, moves) start tile
    where start = (3, height + 4)
          width = 1 + maximum (map fst tile)

stack :: (M.Map (Int,Int) Bool, Int, String) -> [[(Int,Int)]] -> [Int]
stack start tiles = map (\(m,h,_) -> h) stream
    where stream = scanl play start tiles

loops :: [Int] -> Int ->  [(Int,Int)]
loops heights gap
    | length (group $ take 10 diffs) == 1 = [(gap, head diffs)]
    | otherwise = []
    where skips [] = []
          skips t = (head t) : (skips $ drop gap t)
          differ (a:b:cs) = (b-a) : differ (b:cs)
          diffs = differ $ skips $ drop gap heights

process :: String -> [String]
process s = map show [tower !! 2022, toplen]
    where tower = stack (M.empty, 0, cycle s) (cycle tileset)
          (llen, ldiff) = head $ concatMap (loops tower.(*(length tileset))) [1..]
          (biglen, rest) = divMod (1000000000000) llen
          toplen = (tower !! (llen+rest)) + ((biglen - 1) * ldiff)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . head . lines)

