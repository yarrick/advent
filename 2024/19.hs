import Data.Char
import Data.List
import qualified Data.Map as M

process (towels,patterns) = map show [length $ filter (>0) res, sum res]
    where tgroups = groupBy (\a b -> head a == head b) $ sort towels
          res = map (snd.build M.empty tgroups) patterns

build :: M.Map String Int -> [[String]] -> String -> (M.Map String Int, Int)
build cache tgroups [] = (cache, 1)
build cache tgroups word
    | M.member word cache = (cache, cache M.! word)
    | length starting == 0 || length matching == 0 = (M.insert word 0 cache, 0)
    | otherwise = (newcache, newcache M.! word)
    where starting = filter (\tg -> head (head tg) == head word) tgroups
          matching = filter (\s -> s == take (length s) word) $ head starting
          descend c n [] = M.insert word n c
          descend c n (s:ss) = descend nc (n+res) ss
                where (nc, res) = build c tgroups (drop (length s) word)
          newcache = descend cache 0 matching

parse :: [String] -> ([String], [String])
parse (parts:blank:patterns) = (map (filter isAlpha) $ words parts, patterns)

main :: IO ()
main = interact (unlines . process . parse . lines)
