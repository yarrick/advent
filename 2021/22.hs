import Data.Char
import Data.List

type Range = (Int,Int) -- start,end (both inclusive)
data Dim = D (Range,[Dim]) deriving (Eq,Show)

reboot :: [Dim] -> (Bool,[Range]) -> [Dim]
reboot [] (_,[]) = []
reboot [] (False,(r:[])) = []
reboot d@(D ((ri,rj),[]):ds) (False,((ra,rb):[]))
    | rb < ri = d
    | ra < ri && rb >= ri = reboot d (False,((ri,rb):[]))
    | ra == ri && rb < rj = D ((succ rb,rj),[]) : ds
    | ra == ri && rb == rj = ds
    | ra == ri && rb > rj = reboot ds (False,(succ rj,rb):[])
    | ra > ri && ra < rj = D ((ri,pred ra),[]) : reboot (D ((ra,rj),[]):ds) (False,((ra,rb):[]))
reboot [] (b,(r:rs)) = [D (r,reboot [] (b,rs))]
reboot d@(D ((ri,rj),rd):ds) op@(b,((ra,rb):rs))
    | rb < ri = reboot [] op ++ d
    | ra < ri && rb >= ri = reboot [] (b,((ra,pred ri):rs)) ++ reboot d (b,((ri,rb):rs))
    | ra == ri && rb < rj = D ((ri,rb),reboot rd (b,rs)) : D ((succ rb,rj),rd) : ds
    | ra == ri && rb == rj = D ((ri,rj),reboot rd (b,rs)) : ds
    | ra == ri && rb > rj = D ((ri,rj),reboot rd (b,rs)) : reboot ds (b,(succ rj,rb):rs)
    | ra > ri && ra <= rj = D ((ri,pred ra),rd) : reboot (D ((ra,rj),rd):ds) (b,((ra,rb):rs))
    | ra > rj = D ((ri,rj),rd) : reboot ds op

count :: [Dim] -> Int
count ds = sum $ map (\(D ((xa,xb),yy)) -> ((county yy) * (xb-xa+1))) ds
    where county ys = sum $ map (\(D ((ya,yb),zz)) -> (countz zz) * (yb-ya+1)) ys
          countz zs = sum $ map (\(D ((za,zb),_)) -> zb-za+1) zs

process :: [(Bool,[Range])] -> [String]
process ops =  map (show.lit) [filter tiny ops, ops]
    where tiny (_,rs) = all (\(a,b) -> small a && small b) rs
          small n = abs n <= 50
          lit o = count $ foldl reboot [] o

parse :: String -> (Bool,[Range])
parse str = (take 2 (head parts) == "on", pairs oddnums)
    where isnum c = isDigit c || c == '-'
          parts = groupBy (\a b -> isnum a == isnum b) str
          oddnums = map (read.snd) $ filter (odd.fst) $ zip [0..] parts
          pairs (a:b:cs) = (a,b) : pairs cs
          pairs [] = []

main :: IO ()
main = interact (unlines . process . map parse . lines)
