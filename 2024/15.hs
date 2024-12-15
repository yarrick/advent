import qualified Data.Set as S

type Pos = (Int, Int) -- row, col

process :: (Int, Int, Pos, S.Set Pos, S.Set Pos, String) -> [String]
process (nrows, ncols, start, boxes, walls, moves) = [show $ sum $ map gps $ S.toList pushed]
    where (robo, pushed) = foldl (move walls) (start,boxes) moves
          gps (r,c) = 100*r+c

step (r,c) '^' = (r-1,c)
step (r,c) 'v' = (r+1,c)
step (r,c) '>' = (r,c+1)
step (r,c) '<' = (r,c-1)

move walls (robo,boxes) ch
    | S.member npos walls = (robo, boxes)
    | S.member npos boxes && pushedbox == npos = (robo,boxes)
    | S.member npos boxes = (npos,S.insert pushedbox $ S.delete npos nboxes)
    | otherwise = (npos, boxes)
    where npos = step robo ch
          (pushedbox, nboxes) = move walls (npos, boxes) ch


parse :: [String] -> (Int, Int, Pos, S.Set Pos, S.Set Pos, String)
parse ss = (length grid, length (head grid), head $ select '@',
            S.fromList $ select 'O', S.fromList $ select '#', concat moves)
    where (grid,(blank:moves)) = break (""==) ss
          inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) grid
          select ch = map fst $ filter (\(a,b) -> b == ch) cells

main :: IO ()
main = interact (unlines . process . parse . lines)
