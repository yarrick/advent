import qualified Data.Set as S
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col

process :: ([String], String) -> [String]
process (grid, moves) = map (show.sum.map gps.M.keys) [pushed1, pushed2]
    where pushed1 = push grid moves
          pushed2 = M.filter ('['==) $ push (map (concatMap expand) grid) moves
          gps (r,c) = 100*r+c
          expand ch
            | ch == 'O' = "[]"
            | ch == '@' = "@."
            | otherwise = ch : ch : []

step (r,c) '^' = (r-1,c)
step (r,c) 'v' = (r+1,c)
step (r,c) '>' = (r,c+1)
step (r,c) '<' = (r,c-1)

move walls (robo,boxes) ch
    | S.member npos walls = (robo, boxes)
    | M.member npos boxes && pushedbox == npos = (robo,boxes)
    | M.member npos boxes && (boxchar == 'O' || elem ch "<>") =
        (npos,M.insert pushedbox boxchar $ M.delete npos nboxes)
    | M.member npos boxes && pushedpeer == peerbox = (robo,boxes)
    | M.member npos boxes =
        (npos, M.insert pushedbox boxchar $ M.delete npos $
               M.insert pushedpeer (boxes M.! peerbox) $ M.delete peerbox nnboxes)
    | otherwise = (npos, boxes)
    where npos = step robo ch
          boxchar = boxes M.! npos
          peerbox
            | boxchar == '[' = (fst npos, snd npos +1)
            | otherwise = (fst npos, snd npos -1)
          (pushedbox, nboxes) = move walls (npos, boxes) ch
          (pushedpeer, nnboxes) = move walls (peerbox, nboxes) ch

push :: [String] -> String -> M.Map Pos Char
push grid moves = snd $ foldl (move $ S.fromList $ map fst $ select "#")
                        (fst $ head $ select "@", M.fromList $ select "O[]") moves
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs
          cells = concatMap inject $ zip [0..] $ map (zip [0..]) grid
          select ch = filter (\(a,b) -> elem b ch) cells

parse :: [String] -> ([String], String)
parse ss = (grid, concat moves)
    where (grid,(blank:moves)) = break (""==) ss

main :: IO ()
main = interact (unlines . process . parse . lines)
