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

getBot :: String -> Bot
getBot str = Bot {bid=read $ args!!1,c1=Empty,c2=Empty,loD=getDest args 5,hiD=getDest args 10}
  where args = words str

addChip :: [Bot] -> (Chip,Int) -> [Bot]
addChip [] _ = []
addChip (b:bb) (chip,bbid)
 | bbid == bid b && Empty == c1 b = Bot {bid=bid b, c1=chip,c2=c2 b,loD=loD b,hiD=hiD b} : bb
 | bbid == bid b = Bot {bid=bid b, c1=c1 b,c2=chip,loD=loD b,hiD=hiD b} : bb
 | otherwise = b : addChip bb (chip,bbid)

getChip :: String -> (Chip,Int)
getChip str = (Chip $ read $ args !! 1, read $ args !! 5)
  where args = words str

doBot :: ([(Int,Int)],[Bot]) -> Chip -> Dest -> ([(Int,Int)],[Bot])
doBot (outs,bots) (Chip c) (DOut d) = ((d,c) : outs,bots)
doBot (outs,bots) chip (DBot b) = (outs, addChip bots (chip,b))

moveChip :: ([(Int,Int)],[Bot]) -> Bot -> ([(Int,Int)],[Bot])
moveChip (outs,bots) b = doBot (doBot (outs,bots) (head chips) (loD b)) (last chips) (hiD b)
  where chips = sort $ [c1 b, c2 b]

part1 :: [Bot] -> Bot
part1 bots
 | sort [c1 bb, c2 bb] == [Chip 17, Chip 61] = bb
 | otherwise = part1 $ filter (bb/=) $ snd $ moveChip ([],bots) bb
  where bb = head $ filter (\a -> c1 a /= Empty && c2 a /= Empty) bots

part2 :: ([(Int,Int)],[Bot]) -> ([(Int,Int)],[Bot])
part2 (outs,[]) = (outs,[])
part2 (outs,bots) = part2 (o2, filter (bb/=) b2)
  where bb = head $ filter (\a -> c1 a /= Empty && c2 a /= Empty) bots
        (o2,b2) = moveChip (outs,bots) bb

process :: [String] -> [String]
process rows = map show [bid $ part1 start,
                         product $ map snd $ take 3 $ sort $ fst $ part2 ([],start)]
  where (botlines,vallines) = rowtypes ([],[]) rows
        start = foldl addChip (map getBot botlines) $ map getChip vallines

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)
