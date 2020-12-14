import Data.Bits
import Data.Char
import Data.List

data Cmd = Mask String | Write Integer Integer deriving (Eq,Show)

asNum :: (Char,Integer,Integer) -> String -> Integer
asNum _ [] = 0
asNum n@(a,an,bn) (c:cs)
    | c == a = an + 2 * (asNum n cs)
    | otherwise = bn + 2 * (asNum n cs)

asBin :: Integer -> [Integer]
asBin n = (take (36 - length res) $ cycle [0]) ++ res
    where conv 0 = []
          conv n = (mod n 2) : conv (div n 2)
          res = (reverse (conv n))

valMask :: String -> Integer -> Integer
valMask m n = (n .|. (asNum ('1',1,0) $reverse m)) .&. (asNum ('0',0,1) $ reverse m)

addrMask :: String -> [Integer] -> [[Integer]]
addrMask [] [] = [[]]
addrMask ('0':mm) (p:ps) = map (\l -> p : l) $ addrMask mm ps
addrMask ('1':mm) (p:ps) = map (\l -> 1 : l) $ addrMask mm ps
addrMask ('X':mm) (p:ps) = concatMap (\l -> [(1:l), (0:l)]) $ addrMask mm ps

parse :: [String] -> Cmd
parse ("mask":"=":mm:xs) = Mask mm
parse (wr:"=":num:xs) = Write (read $ filter isDigit wr) (read num)

run :: [Cmd] -> String -> [(Integer,Integer)] -> [(Integer,Integer)]
run [] _ a = a
run ((Mask mm):cs) _ mem = run cs mm mem
run ((Write pos val):cs) currmask mem = run cs currmask $ (pos,mval) : omem
    where mval = valMask currmask val
          omem = filter (\(p,_) -> p /= pos) mem

run2 :: [Cmd] -> String -> [(String,Integer)] -> [(String,Integer)]
run2 [] _ a = a
run2 ((Mask mm):cs) _ mem = run2 cs mm mem
run2 ((Write pos val):cs) currmask mem = run2 cs currmask (newmem++oldmem)
    where poss = map (concatMap show) $ addrMask currmask $ asBin pos
          newmem = zip poss (cycle [val])
          (_,oldmem) = partition (\(k,_) -> elem k poss) mem

process :: [String] -> [String]
process rows = map (show.sum) [map snd $ run cmds "X" [], map snd $ run2 cmds "X" [] ]
    where cmds = map (parse.words) rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

