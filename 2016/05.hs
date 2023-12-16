import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Data.List

md5sum :: String -> String
md5sum str = show $ md5 $ B.pack str
    where md5 s = hash s :: Digest MD5

bruteforce :: ([String],Int,Int) -> String -> ([String],Int,Int)
bruteforce (code,count,goal) door
 | (length code) == goal = (code,count,goal)
 | take 5 try == "00000" = bruteforce (code ++ [try],count+1,goal) door
 | otherwise = bruteforce (code,count+1,goal) door
  where try = md5sum $ door ++ show count

part1 :: String -> String
part1 door = getCode $ bruteforce ([],0,8) door
  where getCode (a,b,c) = map (!!5) a

--part 2
conv :: [(Char,Char)] -> [(Int,Char)]
conv [] = []
conv ((a,b):cc)
 | a >= '0' && a <= '7' = (read [a],b) : conv cc
 | otherwise = conv cc

cmp :: (Int,Char) -> (Int,Char) -> Ordering
cmp (pa,ca) (pb,cb) = compare pa pb

merge :: (String,Int) -> [(Int,Char)] -> (String,Int)
merge a [] = a
merge (code,prev) ((pos,c):cc)
 | pos == prev + 1 = merge (code ++ [c],pos) cc
 | otherwise = merge (code,prev) cc

part2 :: String -> String
part2 door = fst $ merge ("",-1) $ sortBy cmp $ conv $ getCode $ bruteforce ([],0,34) door
  where getCode (a,b,c) = map (\x -> (x!!5,x!!6)) a

process (row:_) = [part1 row, part2 row]

main :: IO ()
main = interact (unlines . process . lines)
