import Data.ByteString.Char8 as BS
import Crypto.Hash.MD5 as MD5
import Data.Char
import Text.Printf

hexify :: String -> String
hexify [] = []
hexify (c:cc) = printf "%02X" (ord c) ++ hexify cc

md5 :: String -> String
md5 str = hexify $ BS.unpack $ MD5.hash $ BS.pack str

search :: String -> Integer -> Integer
search prefix num
	| Prelude.take 5 md5sum == "00000" = num
	| otherwise = search prefix (num + 1)
	where
		md5sum = md5 $ prefix ++ show num

--part 2

searchx :: String -> Integer -> Int -> Integer
searchx prefix num count
	| Prelude.take count md5sum == zeros = num
	| otherwise = searchx prefix (num + 1) count
	where
		md5sum = md5 $ prefix ++ show num
		zeros = Prelude.take count $ repeat '0'
