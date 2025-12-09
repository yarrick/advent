import qualified Data.Set as S
import Data.List

process rows = map show $ beam beams (S.fromList $ celltypes '^') maxrow 0
    where rcells (r,row) = [((r,c),s) | (c,s) <- zip [1..] row ]
          cells = concatMap rcells $ zip [1..] rows
          maxrow = fst $ fst $ last cells
          celltypes c = map fst $ filter (\(p,v) -> v == c) cells
          beams = zip (celltypes 'S') (cycle [1])

beam beams splitters end splits
    | any (==end) $ map (fst.fst) beams = [splits, sum $ map snd beams]
    | otherwise = beam (hits ++ open) splitters end (splits + length hit)
    where drop ((r,c),v) = ((r+1,c), v)
          (hit, open) = partition (\b -> S.member (fst b) splitters) $ map drop beams
          hits = combine $ sort $ concatMap (\((r,c),v) -> [((r,c-1),v), ((r,c+1),v)]) hit
          combine [] = []
          combine [a] = [a]
          combine ((pa,va):(pb,vb):ps)
            | pa == pb = combine ((pa,va+vb):ps)
            | otherwise = (pa,va) : combine ((pb,vb):ps)

main :: IO ()
main = interact (unlines . process . lines)
