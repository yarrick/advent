import Data.List

data Dest = DBot Int | DOut Int deriving (Show, Eq)
data Chip = Chip Int | Empty deriving (Show, Eq, Ord)
data Bot = Bot {
  bid ::Int,
  c1 ::Chip,
  c2 ::Chip,
  loD::Dest,
  hiD::Dest
} deriving (Show,Eq)

rowtypes :: ([String],[String]) -> [String] -> ([String],[String])
rowtypes a [] = a
rowtypes (b,c) (a:bb)
 | head a == 'b' = rowtypes (a : b,c) bb
 | head a == 'v' = rowtypes (b, a : c) bb

getDest :: [String] -> Int -> Dest
getDest args start
 | h == 'b' = DBot num
 | otherwise = DOut num
  where h = head $ args !! start
        num = read $ args !! (start+1)

getBots :: [String] -> [Bot]
getBots [] = []
getBots (a:bb) = Bot {bid=read $ args!!1,c1=Empty,c2=Empty,
     loD=getDest args 5,hiD=getDest args 10} : getBots bb
  where args = words a

addChip :: [Bot] -> (Chip,Int) -> [Bot]
addChip [] _ = []
addChip (b:bb) (chip,bbid)
 | bbid == bid b && Empty == c1 b = Bot {bid=bid b, c1=chip,c2=c2 b,loD=loD b,hiD=hiD b} : bb
 | bbid == bid b = Bot {bid=bid b, c1=c1 b,c2=chip,loD=loD b,hiD=hiD b} : bb
 | otherwise = b : addChip bb (chip,bbid)

getChip :: String -> (Chip,Int)
getChip str = (Chip $ read $ args !! 1, read $ args !! 5)
  where args = words str

firstFull :: [Bot] -> Bot
firstFull (a:bb)
 | c1 a /= Empty && c2 a /= Empty = a
 | otherwise = firstFull bb

moveChip :: [Bot] -> Bot -> [Bot]
moveChip bots b = addChip (addChip bots (head chips, dest $ loD b)) (last chips, dest $ hiD b)
  where chips = sort $ [c1 b, c2 b]
        dest (DBot d) = d
        dest (DOut _) = -1

part1 :: [Bot] -> Bot
part1 bots
 | sort [c1 bb, c2 bb] == [Chip 17, Chip 61] = bb
 | otherwise = part1 $ filter (bb/=) $ moveChip bots bb
  where bb = firstFull bots

process :: [String] -> [String]
process rows = [show $ bid $ part1 start]
  where (botlines,vallines) = rowtypes ([],[]) rows
        start = foldl addChip (getBots botlines) $ map getChip vallines

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
