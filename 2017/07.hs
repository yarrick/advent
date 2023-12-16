import Data.List
import Data.Char

data Item = Program String Int [String] [Item] deriving (Show, Eq, Ord)

parse :: [String] -> Item
parse (p:val:ps) = Program p num deps []
    where num = read $ takeWhile isDigit $ tail val
          deps = map (takeWhile isAlpha) $ drop 1 ps

tryAdd :: [Item] -> Item -> [Item]
tryAdd unowned (Program name val owns has)
    | length taken == length owns = (Program name val [] taken) : left
    | otherwise = unowned
    where owned (Program n _ _ _) = elem n owns
          (taken,left) = partition owned unowned

build :: ([Item], [Item], [Item]) -> ([Item], [Item], [Item])
build (a,b,[]) = (a,b,[])
build (unowned, tried, (i:is))
    | added == unowned = build (unowned, i:tried, is)
    | otherwise = build (added, tried, is)
    where added = tryAdd unowned i

construct :: [Item] -> [Item] -> [Item]
construct unowned owners
    | retry == [] = newUnowned
    | otherwise = construct newUnowned retry
    where (newUnowned,retry,_) = build (unowned,[],owners)

balanced :: Item -> Int
balanced (Program _ _ _ is)
    | length outlier == 0 = -1
    | next == -1 = target - (subWeight item)
    | otherwise = next
    where weight (Program _ w _ is) = w + sum (map weight is)
          weighted = [(weight i,i) | i <- is ]
          target = fst $ (sort weighted) !! 1
          outlier = filter (\(w,_) -> w /= target) weighted
          (ww,item) = head outlier
          subWeight (Program _ _ _ ss) = sum (map weight ss)
          next = balanced item

process :: [String] -> [String]
process rows = [name top, show $ balanced top]
    where apps = map (parse . words) rows
          noDeps (Program _ _ t _) = length t == 0
          (unowned, owners) = partition noDeps apps
          name (Program n _ _ _) = n
          top = head $ construct unowned owners

main :: IO ()
main = interact (unlines . process . lines)
