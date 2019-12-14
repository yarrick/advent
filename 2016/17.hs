import Prelude hiding (Left,Right)
import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import Data.List

data Direction = Up | Down | Left | Right deriving (Show, Eq)

md5 :: String -> String
md5 str = C8.unpack $ encode $ hashlazy $ toLazyByteString $ string8 str

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

run key = snd $ head $ sort [ (length p, p) | p <- paths key ((0,0),"") ]
run2 key = length $ snd $ head $ reverse $ sort [ (length p, p) | p <- paths key ((0,0),"") ]
