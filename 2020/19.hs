import Data.Char
import Data.List

data Rule = Ref [[Int]] | Part Char | Expanded [String] deriving (Eq,Show)
type Rules = [(Int,Rule)]

allowed :: [String] -> [Int] -> [[Int]]
allowed [] a = [a]
allowed ("|":ss) a = a : allowed ss []
allowed (s:ss) a = allowed ss (a ++ [read s])

parse :: Rules -> [String] -> (Rules, [String])
parse r ("":ss) = (r,ss)
parse r (s:ss)
    | length tokens == 2 && head char == '"' = parse ((tag, Part (char !! 1)) : r) ss
    | otherwise = parse ((tag, Ref $ allowed (tail tokens) []) : r) ss
    where tokens = words s
          tag = read $ takeWhile isDigit $ head tokens
          char = tokens !! 1

isPart :: (Int,Rule) -> Bool
isPart (_,Part _) = True
isPart _ = False

isKnown :: Rules -> (Int,Rule) -> Bool
isKnown parts (tag,Ref choices) = all (all hasTag) choices
    where hasTag x = elem x (map fst parts)

fetch :: Rules -> Int -> Rule
fetch rs tag = snd $ head $ filter (\(t,_) -> t == tag) rs

prefix ss word = map (\w -> word++w) ss

combine :: [String] -> [[String]] -> [String]
combine path ([a]:as) = combine (prefix path a) as
combine path [] = path
combine path (a:as) = combine (concatMap (prefix path) a) as

expand :: Rules -> (Int,Rule) -> (Int,Rule)
expand known (tag, Ref choices) = (tag, Expanded $ concatMap build choices)
    where chunk (Part c) = [[c]]
          chunk (Expanded s) = s
          build n = combine [""] $ map (chunk.fetch known) n

-- args: todo, attempted, resolved
resolve :: (Rules,Rules,Rules) -> (Rules,Rules,Rules)
resolve ([],a,b) = ([],a,b)
resolve (r:rs,att,kn)
    | isKnown kn r = resolve (rs, att, (expand kn r):kn)
    | otherwise = resolve (rs, r:att, kn)

solve :: (Rules,Rules,Rules) -> Int -> [String]
solve (_,refs,parts) goal
    | target == [] = solve ([], todo, done) goal
    -- constructed strings are backwards..
    | otherwise = map reverse $ contents $ snd $ head target
    where (_,todo,done) = resolve (refs,[],parts)
          contents (Expanded a) = a
          target = filter (\(tag,_) -> goal == tag) done

part2 :: [String] -> [String] -> [String] -> Int
part2 _ _ [] = 0
part2 a31 a42 (w:ws)
    | mod (length w) wlen /= 0 = part2 a31 a42 ws
    | length fits /= 2 = part2 a31 a42 ws
    | good fits = 1 + part2 a31 a42 ws
    | otherwise = part2 a31 a42 ws
    where wlen = length $ head a31
          chunks [] = []
          chunks r = (take wlen r) : chunks (drop wlen r)
          fits = map (\a -> (head a, length a)) $ group $ map (\x -> (elem x a31, elem x a42)) $ chunks w
          good (((False,True),n42):((True,False),n31):[]) = n42 > n31
          good _ = False

process :: (Rules, [String]) -> [String]
process (m,ws) = map show [length $ filter (\w -> elem w allowed0) ws, part2 allowed31 allowed42 ws]
    where (parts, refs) = partition isPart m
          allowed0 = solve ([], refs, parts) 0
          allowed31 = solve ([], refs, parts) 31
          allowed42 = solve ([], refs, parts) 42


main :: IO ()
main = interact (unlines . process . parse [] . lines)
