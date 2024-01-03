import Data.Char
import Data.List

type Level = (Int,[Int])

parse :: [String] -> Level
parse (lr:d:ss) = (read lvl, lvlpos $ read d)
    where lvl = takeWhile isDigit lr

lvlpos d = d : replicate (2*(d-1) - 1) 0

levels :: Int -> [Level] -> [Level]
levels _ [] = []
levels d (l@(lr,_):lvls)
    | d == lr = l : levels (succ d) lvls
    | otherwise = (d, [0]) : levels (succ d) (l:lvls)

run :: Int -> [Level] -> Int
run _ [] = 0
run delay ((d,lpos):lvls) = d * ((cycle lpos) !! delay) + run (succ delay) lvls

pass :: Level -> Int -> Bool
pass (lr,lvl) delay = (lvl) !! n == 0
    where n = mod (delay+lr) (length lvl)

grow :: (Int,[Int]) -> Int -> [Int]
grow (n,passes) size = sort [ (a+n*b) | a <- passes, b <- [0..factor-1]]
    where factor = quot size n

gap :: (Int,[Int]) -> Level -> (Int,[Int])
gap (len,passes) lvl = (size, filter (pass lvl) grown)
    where size = lcm len (length $ snd lvl)
          grown = grow (len,passes) size

process :: [String] -> [String]
process rows = map show [run 0 lvls, head gaps]
    where lvls = levels 0 $ map (parse . words) rows
          (_,gaps) = foldl gap (1,[0]) lvls

main :: IO ()
main = interact (unlines . process . lines)
