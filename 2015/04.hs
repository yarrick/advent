import qualified Data.ByteString.Char8 as BS
import Crypto.Hash
import Data.Char

md5 :: String -> String
md5 str = show $ md5 $ BS.pack str
    where md5 s = hash s :: Digest MD5

searchx :: String -> Integer -> Int -> Integer
searchx prefix num count
    | take count md5sum == zeros = num
    | otherwise = searchx prefix (num + 1) count
    where
        md5sum = md5 $ prefix ++ show num
        zeros = take count $ repeat '0'

process (row:_) = map show [searchx row 0 5, searchx row 0 6]

main :: IO ()
main = interact (unlines . process . lines)
