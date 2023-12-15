import Data.List
import Data.Maybe

process :: [String] -> [String]
process dish = map (show.sum.load) [north dish, spun !! (loop1 + cycles)]
    where score (n,s) = n * length (filter ('O'==) s)
          load s = map score $ zip [1..] $ reverse s
          spun = iterate (east . south . west . north) dish
          loops old ((n,d):ds)
            | isJust ix = (n, (fromJust ix)) : loops [d] ds
            | otherwise = loops (old++[d]) ds
            where ix = elemIndex d old
          (loop1,repeated):(loop2,_):_ = loops [] $ zip [0..] spun
          cycles = (1000000000 - loop1) `mod` (loop2 - loop1)

north = transpose . west . transpose
south = reverse . north . reverse
west = map (reverse . right . reverse)
east = map right

right :: String -> String
right [] = []
right s = (sort free) ++ (take 1 stuck) ++ right (drop 1 stuck)
    where (free,stuck) = break ('#'==) s

main :: IO ()
main = interact (unlines . process . lines)
