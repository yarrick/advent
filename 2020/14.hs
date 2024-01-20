import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as M

data Cmd = Mask String | Write Int Int deriving (Eq,Show)

asNum :: (Char,Int,Int) -> String -> Int
asNum _ [] = 0
asNum n@(a,an,bn) (c:cs)
    | c == a = an + 2 * (asNum n cs)
    | otherwise = bn + 2 * (asNum n cs)

asBin :: Int -> [Int]
asBin n = (take (36 - length res) $ cycle [0]) ++ res
    where conv 0 = []
          conv n = (mod n 2) : conv (div n 2)
          res = (reverse (conv n))

valMask :: String -> Int -> Int
valMask m n = (n .|. (asNum ('1',1,0) $ reverse m)) .&. (asNum ('0',0,1) $ reverse m)

addrMask :: String -> [Int] -> [[Int]]
addrMask [] [] = [[]]
addrMask ('0':mm) (p:ps) = map (\l -> p : l) $ addrMask mm ps
addrMask ('1':mm) (p:ps) = map (\l -> 1 : l) $ addrMask mm ps
addrMask ('X':mm) (p:ps) = concatMap (\l -> [(1:l), (0:l)]) $ addrMask mm ps

parse :: [String] -> Cmd
parse ("mask":"=":mm:xs) = Mask mm
parse (wr:"=":num:xs) = Write (read $ filter isDigit wr) (read num)

run :: [Cmd] -> String -> M.Map Int Int -> [Int]
run [] _ a = M.elems a
run ((Mask mm):cs) _ mem = run cs mm mem
run ((Write pos val):cs) currmask mem = run cs currmask $ M.insert pos mval mem
    where mval = valMask currmask val

run2 :: [Cmd] -> String -> M.Map String Int -> [Int]
run2 [] _ a = M.elems a
run2 ((Mask mm):cs) _ mem
    | length (filter ('X'==) mm) > 20 = [] -- avoid exploding on first example
    | otherwise = run2 cs mm mem
run2 ((Write pos val):cs) currmask mem = run2 cs currmask $ foldl update mem poss
    where poss = map (concatMap show) $ addrMask currmask $ asBin pos
          update m k = M.insert k val m

process :: [String] -> [String]
process rows = map (show.sum) [run cmds "" M.empty, run2 cmds "" M.empty]
    where cmds = map (parse.words) rows

main :: IO ()
main = interact (unlines . process . lines)
