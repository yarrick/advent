
dragon :: String -> Int -> String
dragon a minlen
  | length next >= minlen = take minlen next
  | otherwise = dragon next minlen
  where b = invert $ reverse a
        next = a ++ "0" ++ b

invert :: String -> String
invert [] = []
invert ('1':is) = '0' : invert is
invert ('0':is) = '1' : invert is

chksum :: String -> String
chksum [] = []
chksum (a:b:cs)
  | a == b = '1' : chksum cs
  | otherwise = '0' : chksum cs

csum :: String -> String
csum a
  | odd $ length first = first
  | otherwise = csum first
  where first = chksum a

run str len = csum $ dragon str len

process :: [String] -> [String]
process (row:_) = [run row 272, run row 35651584]

main :: IO ()
main = interact (unlines . process . lines)
