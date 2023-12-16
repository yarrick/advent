import Prelude hiding (Left,Right)
import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import Data.List

data Direction = Up | Down | Left | Right deriving (Show, Eq)

md5 :: String -> String
md5 str = show $ md5hash $ B.pack str
    where md5hash s = hash s :: Digest MD5

open :: String -> String -> [Direction]
open key path = map fst $ filter snd $ zip [Up,Down,Left,Right] $ map opened $ take 4 $ md5 (key ++ path)
  where opened c = c >= 'b' && c <= 'f'

pass :: ((Int,Int),String) -> Direction -> ((Int,Int),String)
pass ((x,y),p) Up = ((x, y - 1), p ++ "U")
pass ((x,y),p) Down = ((x, y + 1), p ++ "D")
pass ((x,y),p) Left = ((x - 1, y), p ++ "L")
pass ((x,y),p) Right = ((x + 1, y), p ++ "R")

paths :: String -> ((Int,Int),String) -> [String]
paths key cur@(pos,path)
  | pos == (3,3) = [path]
  | otherwise = concat $ map (paths key) p
  where p = filter (\((x,y),_) -> valid x && valid y) $ map (pass cur) $ open key path
        valid n = n >= 0 && n < 4

process (row:_) = [snd $ head walked, show $ length $ snd $ head $ reverse walked]
    where walked = sort [ (length p, p) | p <- paths row ((0,0),"") ]

main :: IO ()
main = interact (unlines . process . lines)
