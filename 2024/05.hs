import Data.List
import Data.Maybe

process :: ([(Int,Int)], [[Int]]) -> [String]
process (rules,jobs) = map (show.sum.map midpage.map snd) [valid, map reorder invalid]
    where active j = filter (\(a,b) -> elem a j && elem b j) rules
          rulejobs = map (\j -> (active j, j)) jobs
          (valid,invalid) = partition check rulejobs
          midpage j = j !! (div (length j) 2)

check (rules, job) = all (\(a,b) -> pos a < pos b) rules
    where pos n = fromJust (findIndex (==n) job)

reorder (rules, job) = (rules, sorter rules job)
    where sorter _ [] = []
          sorter rs j = early ++ sorter (filter (\(a,b) -> elem a late) rs) late
            where (before,after) = unzip rs
                  (early,late) = partition (\n -> notElem n after) j

parse ss = (map dep regs, map tolist $ tail rest)
    where (regs, rest) = break (""==) ss
          dep s = (read a, read $ tail b)
            where (a,b) = break ('|'==) s
          tolist s = read $ ['['] ++ s ++ [']']

main :: IO ()
main = interact (unlines . process . parse . lines)
