import Data.List

-- id, topleft, size
type Claim = (Int, (Int,Int), (Int,Int))

parse :: String -> Claim
parse str = (read $ tail cid,(read col,read $ take (length row - 2) $ tail row),(read width,read $ tail xheight))
  where [cid,_,start,size] = words str
        (width,xheight) = break ('x'==) size
        (col,row) = break (','==) start

squares :: Claim -> [((Int,Int),Int)]
squares (cid,(col,row),(width,height)) =
  [ ((i,j),cid) | i <- [row..(row+height-1)], j <- [col..(col+width-1)] ]

dups :: (Int,[Int]) -> [((Int,Int),Int)] -> [Int]
dups (n,ids) [a] = [n,head ids]
dups (n,ids) cs@(((ia,ja),_):s@((ib,jb),_):ss)
  | ia /= ib || ja /= jb = dups (n,ids) (s:ss)
  | otherwise = dups (n+1,nids) (dropWhile (samepos (ia,ja)) cs)
  where samepos (c,r) ((i,j),_) = c == i && r == j
        nids = foldl (\cids dup -> delete dup cids) ids $ map snd $ takeWhile (samepos (ia,ja)) cs

process :: [String] -> [String]
process rows = map show $ dups (0,claimids) $ sort $ concatMap squares claims
  where claims = map parse rows
        claimids = map (\(cid,_,_) -> cid) claims

main :: IO ()
main = interact (unlines . process . lines)
