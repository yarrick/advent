data Tree = Node [Int] [Tree] deriving (Eq,Show)

children :: Int -> [Int] -> [Tree] -> ([Tree],[Int])
children 0 inp ch = (ch,inp)
children n inp ch = children (n-1) inpp (ch++[c])
  where (c,inpp) = build inp

build :: [Int] -> (Tree,[Int])
build (childlen:mdlen:xs) = (Node (take mdlen inpp) ch,drop mdlen inpp)
  where (ch,inpp) = children childlen xs []

mdsum :: Tree -> Int
mdsum (Node md child) = sum md + sum (map mdsum child)

value :: Tree -> Int
value (Node md []) = sum md
value (Node md ch) = sum $ map (\n -> value (ch!!(n-1))) refs
  where refs = filter (\c -> c > 0 && c <= length ch) md

process :: String -> [String]
process row
  | length restdata > 0 = ["Got restdata! " ++ show restdata]
  | otherwise = map show [mdsum tree, value tree]
  where inp = map read $ words row
        (tree,restdata) = build inp

-- long file, lets do IO
main :: IO ()
main = interact (unlines . (concatMap process) . lines)
