import Data.Char
import Data.Matrix

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (r,c) (dr,dc) = (r+dr, c+dc)

seen :: Matrix Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
seen m height (r,c) (dr,dc)
    | getElem r c m >= height = False
    | r == 1 || r == nrows m = True
    | c == 1 || c == ncols m = True
    | otherwise = seen m height (move (r,c) (dr,dc)) (dr,dc)

directions :: [(Int, Int)]
directions = [(-1,0), (0,-1), (1,0), (0,1)]

visible :: Matrix Int -> (Int, Int) -> Bool
visible m (r,c)
    | r == 1 || r == nrows m = True
    | c == 1 || c == ncols m = True
    | otherwise = any id $ map (\dm -> seen m (getElem r c m) (move (r,c) dm) dm) directions

dist :: Matrix Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> Int
dist m height len (r,c) (dr,dc)
    | getElem r c m >= height = succ len
    | r == 1 || r == nrows m = succ len
    | c == 1 || c == ncols m = succ len
    | otherwise = dist m height (succ len) (move (r,c) (dr,dc)) (dr,dc)

process :: [String] -> [String]
process rows = map show [length $ filter (visible m) cells,
                         maximum $ map scenic innercells]
    where m = fromLists $ map (map digitToInt) rows
          cells = [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]
          innercells = [(r,c) | r <- [2..nrows m-1], c <- [2..ncols m-1]]
          sights (r,c) dm = dist m (getElem r c m) 0 (move (r,c) dm) dm
          scenic pos = product $ map (sights pos) directions

main :: IO ()
main = interact (unlines . process . lines)
