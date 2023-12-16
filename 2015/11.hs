straight3 :: String -> Bool
straight3 [] = False
straight3 (a:[]) = False
straight3 (a:b:[]) = False
straight3 (a:b:c:dd)
    | b == succ a && c == succ b = True
    | otherwise = straight3 (b:c:dd)

noiol :: String -> Bool
noiol str = not (elem 'i' str || elem 'o' str || elem 'l' str)

pairs :: String -> Int
pairs [] = 0
pairs (a:[]) = 0
pairs (a:b:cc)
    | a == b = 1 + pairs cc
    | otherwise = pairs (b:cc)

goodpassword :: String -> Bool
goodpassword str = straight3 str && noiol str && pairs str >= 2

step :: String -> String
step [] = []
step (a:bb)
    | a == 'z' = 'a' : step bb
    | otherwise = succ a : bb

next :: String -> String
next str = reverse $ step $ reverse str

run :: String -> String
run str
    | goodpassword str = str
    | otherwise = run $ next str

process (row:_) = [part1, run $ next part1]
    where part1 = run row

main :: IO ()
main = interact (unlines . process . lines)
