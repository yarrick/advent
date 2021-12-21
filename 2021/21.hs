import Data.List

type Player = (Int,Int) -- score, offset on board
type Game = ([Player],(Int,[Int])) -- rolls done, dicevals

turn :: Game -> Game
turn (((sc,p):b:_),(rolls,dice)) = (b:(score,newpos):[],(rolls + 3, ndice))
    where (rolled,ndice) = splitAt 3 dice
          newpos = (p + sum rolled) `mod` 10
          score = newpos + 1 + sc

play :: Int -> Game -> Game
play lim g
    | mp >= lim = g
    | otherwise = play lim $ turn g
    where mp = maximum $ map fst $ fst g

process :: [Int] -> [String]
process offs = [show $ score $ play 1000 g]
    where g = (zip (repeat 0) offs, (0, cycle [1..100]))
          vis (ps,(dr,ds)) = show (ps, (dr, take 10 ds))
          score gg = (minimum $ map fst $ fst gg) * (fst $ snd gg)

parse :: [String] -> [Int]
parse rows = map (pred . read . last . words) rows

-- not so long file, lets do IO anyway
main :: IO ()
main = interact (unlines . process . parse . lines)

