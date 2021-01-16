import Data.Matrix

move :: (Matrix Char, [(Int,Int)]) -> Char -> (Matrix Char, [(Int, Int)])
move mp '^' = mp
move mp '$' = mp
move (m, (r,c):ps) dir
    | dir == ')' = (m, ps) -- pop pos
    | dir == '(' = (m, (r,c):(r,c):ps) -- push pos
    | dir == '|' = (m, (head ps):ps) -- reset pos to pushed
    | dir == 'N' = (setElem '.' (r-2,c) $ setElem '-' (r-1,c) m, (r-2, c):ps)
    | dir == 'S' = (setElem '.' (r+2,c) $ setElem '-' (r+1,c) m, (r+2, c):ps)
    | dir == 'E' = (setElem '.' (r,c+2) $ setElem '|' (r,c+1) m, (r, c+2):ps)
    | dir == 'W' = (setElem '.' (r,c-2) $ setElem '|' (r,c-1) m, (r, c-2):ps)

update :: Matrix Int -> (Int, Int) -> Matrix Int
update m (r,c)
    | m ! (r,c) < 0 = m
    | null open = m
    | doors < m ! (r,c) = setElem doors (r,c) m
    | otherwise = m
    where dirs = [((r-1,c),(r-2,c)), ((r+1,c),(r+2,c)), ((r,c-1),(r,c-2)), ((r,c+1),(r,c+2))]
          open = filter (\(door,_) -> m ! door == -5) dirs
          doors = succ $ minimum $ map (\(_,p) -> m ! p) open

flow :: Matrix Int -> Matrix Int
flow m
    | next == m = m
    | otherwise = flow next
    where cells = [(r,c) | r <- [1..nrows m], c <- [1..ncols m] ]
          next = foldl update m cells

score :: Matrix Char -> Matrix Int
score m = flow $ matrix (nrows m) (ncols m) vals
    where vals p
            | c == 'X' = 0
            | c == '.' = 9999
            | c == '#' = -1
            | otherwise = -5
            where c = m ! p

process :: [String] -> [String]
process rows = map show [maximum rooms, length $ filter (>=1000) rooms]
    where start = setElem 'X' spos $ matrix 210 210 (\n -> '#')
          spos = (105, 105)
          (filled, _) = foldl move (start, [spos]) (head rows)
          rooms = filter (>=0) $ toList $ score filled

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
