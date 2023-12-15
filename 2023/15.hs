import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data Op = Delete | Set Int deriving (Eq,Show)

process :: [String] -> [String]
process ops = map (show.sum) [map hash ops, map score [0..255] ]
    where box = foldl hashmap (V.replicate 256 []) $ map parse ops
          score p = (succ p) * (sum $ zipWith (*) [1..] $ map snd $ box V.! p)

parse :: String -> (String,Op)
parse s
    | rest == "-" = (name, Delete)
    | otherwise = (name, Set $ read $ tail rest)
    where (name,rest) = break (not.isAlphaNum) s

hashmap :: V.Vector [(String,Int)] -> (String,Op) -> V.Vector [(String,Int)]
hashmap vv (name,op) = V.modify (\v -> MV.write v num $ next op) vv
    where num = hash name
          pre = vv V.! num
          next Delete = filter (\(n,_) -> n /= name) pre
          next (Set upd)
            | elem name (map fst pre) = bumped
            | otherwise = pre ++ [(name,upd)]
            where bumped = map (update upd) pre
          update upd (a,b)
            | a == name = (a,upd)
            | otherwise = (a,b)

cut :: String -> [String]
cut [] = []
cut s = w : cut (drop 1 rest)
    where (w,rest) = break (','==) s

hash :: String -> Int
hash s = foldl (\n c -> ((n + ord c) * 17) `mod` 256) 0 s

main :: IO ()
main = interact (unlines . process . concatMap cut . lines)
