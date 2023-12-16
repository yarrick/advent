import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Data.List

md5 :: String -> String
md5 str = show $ md5hash $ B.pack str
    where md5hash s = hash s :: Digest MD5

hexhash :: String -> Int -> String
hexhash salt index = md5 $ salt ++ show index

extract :: (Int,String) -> (Int,[Char],[Char])
extract (index,bytes) = (index, take 1 $ grouplen 3, grouplen 5)
  where
    grouplen ln = map snd $ filter (\(l,c) -> l >= ln) groups
    groups = [ (length g, head g) | g <- group bytes ]

hashes :: (String -> Int -> String) -> String -> [(Int,[Char],[Char])]
hashes hasher salt = filter good $ map extract $ zip [0..] $ map (hasher salt) [0..]
  where good (_,three,fives) = length three + length fives > 0

findkey :: [(Int,[Char],[Char])] -> [Int]
findkey ((pos,three,fives):hs)
  | three == [] = findkey hs
  | hasfive pos (head three) hs = pos : findkey hs
  | otherwise = findkey hs

hasfive :: Int -> Char -> [(Int,[Char],[Char])] -> Bool
hasfive start c ((pos,_,fives):hs)
  | start + 1000 < pos = False
  | elem c fives = True
  | otherwise = hasfive start c hs

keys :: (String -> Int -> String) -> String -> [Int]
keys hasher salt = findkey $ hashes hasher salt

run :: String -> Int
run salt = (keys hexhash salt) !! 63

-- part 2

hash2016 :: String -> Int -> String
hash2016 salt index = hashloop 2016 $ md5 $ salt ++ show index

hashloop 0 h = h
hashloop n h = hashloop (n-1) $ md5 h

run2 :: String -> Int
run2 salt = (keys hash2016 salt) !! 63

process (row:_) = map show [run row, run2 row]

main :: IO ()
main = interact (unlines . process . lines)
