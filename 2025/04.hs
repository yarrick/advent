import qualified Data.Set as S

cull m
    | length removed == 0 = []
    | otherwise = removed : cull (foldr S.delete m removed)
    where around (rr,cc) = [ (r,c) | r <- [rr-1..rr+1], c <- [cc-1..cc+1]]
          neighbors p = filter (\pp -> S.member pp m) $ around p
          removable p = length (neighbors p) <= 4
          removed = filter removable $ S.elems m

process rows = map (show.sum) [take 1 culled, culled]
    where rcells (r,row) = [((r,c),s) | (c,s) <- zip [1..] row ]
          cells = concatMap rcells $ zip [1..] rows
          m = S.fromList $ map fst $ filter (\(p, v) -> v == '@') cells
          culled = map length $ cull m

main :: IO ()
main = interact (unlines . process . lines)
