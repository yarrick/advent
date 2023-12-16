
type Chunk = (Int, Int)

parse :: String -> Chunk
parse str = (read low, read $ tail hi)
  where (low,hi) = break ('-'==) str

blacklist :: [Chunk] -> Chunk -> [Chunk]
blacklist [] _ = []
blacklist ((from,to):bs) (ba,bb)
  | from > bb = (from,to) : bs
  | to < ba = (from,to) : blacklist bs (ba,bb)
  | from < ba && to > bb = (from,ba-1) : (bb+1,to) : bs
  | from < ba && to >= ba = (from,ba-1) : blacklist bs (ba,bb)
  | from >= ba && to > bb = (bb+1,to) : bs
  | to <= bb = blacklist bs (ba,bb)


part1 :: [String] -> Int
part1 rows = fst $ head $ foldl blacklist [(0,4294967295)] $ map parse rows

part2 :: [String] -> Int
part2 rows = sum $ map (\(a,b) -> b-a+1) $ foldl blacklist [(0,4294967295)] $ map parse rows

process :: [String] -> [String]
process rows = map show [part1 rows, part2 rows]

main :: IO ()
main = interact (unlines . process . lines)
