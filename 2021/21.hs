import Data.List
import Control.Monad
import Control.DeepSeq

type Player = (Int,(Int,Int)) -- id, score, offset on board
type Game = ([Player],(Int,[Int])) -- rolls done, dicevals

turn :: Game -> Game
turn (((n,(sc,p)):b:_),(rolls,(d:ds))) = (b:(n,(score,newpos)):[],(rolls + 3, ds))
    where newpos = (p + d) `mod` 10
          score = newpos + 1 + sc

play :: Int -> Game -> Game
play lim g
    | snd (snd g) == [] = g
    | mp >= lim = g
    | otherwise = play lim $ turn g
    where mp = maximum $ map (fst.snd) $ fst g

process :: [Int] -> [String]
process offs = map show [score $ play 1000 $ newg offs $ sumrolls $ cycle [1..100],
                         maximum [p1,p2] ]
    where score gg = (minimum $ map (fst.snd) $ fst gg) * (fst $ snd gg)
          diracs = replicateM 5 sums
          (p1,p2) = sumwins (0,0) $ try offs diracs

newg :: [Int] -> [Int] -> Game
newg offs d = (zip [1..] $ zip (repeat 0) offs, (0, d))

sumrolls d = sum rolled : sumrolls next
    where (rolled,next) = splitAt 3 d

sumwins :: (Int,Int) -> [(Int,Int)] -> (Int,Int)
sumwins r [] = r
sumwins (a,b) ((w,rank):ws) = sumwins (deepseq newscore newscore) ws
    where update pid sc
            | w == pid = sc+rank
            | otherwise = sc
          newscore = (update 1 a, update 2 b)

try :: [Int] -> [[(Int,Int)]] -> [(Int,Int)]
try _ [] = []
try offs (dc:dcs)
    | res == [] = try offs expdc ++ try offs dcs
    | otherwise = res ++ try offs dcs
     where res = tryOne offs dc
           expdc = [ dc ++ [a] | a <- sums ]

tryOne :: [Int] -> [(Int,Int)] -> [(Int,Int)]
tryOne offs ss
    | length winner > 0 = [(fst $ head winner, product inst)]
    | otherwise = []
    where (rolls, inst) = unzip ss
          res = play 21 $ newg offs rolls
          winner = filter (\(_,(s,_)) -> s >= 21) $ fst res

sums = map (\n -> (head n, length n)) $ group $ sort rolls
    where rolls = [ a+b+c | a <- [1..3], b <- [1..3], c <- [1..3] ]

parse :: [String] -> [Int]
parse rows = map (pred . read . last . words) rows

main :: IO ()
main = interact (unlines . process . parse . lines)
