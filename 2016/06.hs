import Data.List

count :: [String] -> [(Char,Int)]
count [] = []
count (a:bb) = (head a, length a) : count bb

decode :: ((Char,Int) -> (Char,Int) -> Ordering) -> String -> Char
decode cmp x = fst $ head $ sortBy cmp $ count $ group $ sort x

process :: [String] -> [String]
process rows = [map (decode most) rows, map (decode least) rows]
  where most (ca,va) (cb,vb) = compare vb va
        least (ca,va) (cb,vb) = compare va vb

main :: IO ()
main = interact (unlines . process . transpose . lines)
