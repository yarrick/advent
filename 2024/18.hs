import qualified Data.Map as M

type Pos = (Int, Int) -- row, col

process blocks = [show $ len part1len, tail $ init $ show $ blocks !! (firstblock-1)]
    where ((w,h), part1len)
            | (maximum $ map fst blocks) <= 6 = ((6,6), 12)
            | otherwise = ((70,70), 1024)
          startdist (0,0) = 0
          startdist _ = 99999
          fullgrid = M.fromList [((x,y), startdist (x,y)) | x <- [0..w], y <- [0..h]]
          len n = (paths (foldr M.delete fullgrid $ take n blocks) [(0,0)]) M.! (w,h)
          firstblock = head $ filter (\n -> len n == 99999) [part1len..length blocks]

paths :: M.Map Pos Int -> [Pos] -> M.Map Pos Int
paths m [] = m
paths m ((x,y):bs) = paths newm $ (bs++closer)
    where neighbors = filter (\p -> M.member p m) [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]
          newdist = succ $ m M.! (x,y)
          closer = map fst $ filter (\(p,d) -> d > newdist) $ map (\n -> (n,m M.!n)) neighbors
          newm = foldr (\p mm -> M.insert p newdist mm) m closer

parse :: String -> Pos
parse ss = read $ "(" ++ ss ++ ")"

main :: IO ()
main = interact (unlines . process . map parse . lines)
