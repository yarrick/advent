import Data.List

parse :: String -> [String]
parse [] = []
parse str = next : parse (drop (length next+1) str)
    where next = takeWhile (\c -> c /= ',') str

walk :: (Int,Int,Int) -> String -> (Int,Int,Int)
walk (n,nw,ne) "n" = (n+1,nw,ne)
walk (n,nw,ne) "s" = (n-1,nw,ne)
walk (n,nw,ne) "nw" = (n,nw+1,ne)
walk (n,nw,ne) "se" = (n,nw-1,ne)
walk (n,nw,ne) "ne" = (n,nw,ne+1)
walk (n,nw,ne) "sw" = (n,nw,ne-1)

shared :: Int -> Int -> Int
shared 0 0 = 0
shared a b
    | signum a /= signum b = 0
    | signum a > 0 = minimum [a,b]
    | otherwise = maximum [a,b]

compress :: (Int,Int,Int) -> (Int,Int,Int)
compress pos@(n,nw,ne)
    | nwne /= 0 = compress (n+nwne,nw-nwne,ne-nwne)
    | nws /= 0 = compress (n+nws,nw-nws,ne-nws)
    | swn /= 0 = compress (n-swn,nw+swn,ne+swn)
    | otherwise = pos
    where nwne = shared nw ne
          nws = shared nw (negate n)
          swn = shared (negate ne) n

process :: [String]  -> [String]
process steps = map show [last dists, maximum dists]
    where distance (a,b,c) = sum $ map abs [a,b,c]
          dists = map (distance . compress) $ scanl walk (0,0,0) steps

main :: IO ()
main = interact (unlines . process . parse . concat . lines)
