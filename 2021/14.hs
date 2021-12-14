import Control.DeepSeq
import Data.List
import qualified Data.Map as M

type Polymer = M.Map String String

transmute :: Polymer -> String -> String
transmute _ (a:[]) = (a:[])
transmute swaps (a:b:cs) = swap ++ (transmute swaps (b:cs))
    where swap = M.findWithDefault " " (a:b:[]) swaps

nextgen :: Polymer -> Polymer -> Polymer
nextgen ref swaps = M.fromList $ map encoded $ M.toList swaps
    where encoded (k,v) = (k, init $ transmute ref (v++[last k]))

counts :: String -> [(Char,Int)]
counts [] = []
counts str = sumcount M.empty chars ++ counts end
    where (main,end) = splitAt 1000 str
          chars = zip main (repeat 1)

tencounts :: Polymer -> M.Map String [(Char,Int)]
tencounts swap = M.fromList $ map cc $ M.toList swap
    where cc (k,v) = (k, sumcount M.empty $ counts v)

score10 :: M.Map String [(Char,Int)] -> String -> [(Char,Int)]
score10 _ (a:[]) = [(a,1)]
score10 _ (a:'-':[]) = []
score10 tcs (a:b:cs) = cc ++ (score10 tcs (b:cs))
    where cc = M.findWithDefault [] (a:b:[]) tcs

sumcount :: M.Map Char Int -> [(Char,Int)] -> [(Char,Int)]
sumcount ps [] = M.toList ps
sumcount ps ((k,v):cs) = sumcount (M.alter (addval v) k ps) cs
    where addval val (Just n) = Just (val + n)
          addval val Nothing = Just val

chunkcount :: M.Map String [(Char,Int)] -> String -> [[(Char,Int)]]
chunkcount ps str
    | end == [] = [score10 ps str]
    | otherwise = (sumcount M.empty $ score10 ps (main ++ [head end, '-'])) : chunkcount ps end
    where (main,end) = splitAt 20480 str

process :: (String, Polymer) -> [String]
process (st,swaps) = map show [ scores $ score10 count10 st,
                                scores $ sumlist40 ]
        where swap10 = (iterate (nextgen swaps) swaps) !! 9
              count10 = tencounts swap10
              t20 = transmute swap10 $ transmute swap10 st
              c20 = tencounts $ nextgen swap10 swap10
              count20 = deepseq c20 c20
              sl40 = concat $ map (sumcount M.empty) $ chunkcount count20 t20
              sumlist40 = deepseq sl40 sl40
              scores cs = (last ranking) - (head ranking)
                where ranking = sort $ map snd $ sumcount M.empty cs

parse :: [String] -> (String, Polymer)
parse (st:_:bs) = (st, M.fromList $ map (swaps.words) bs)
    where swaps (a:_:b:_) = (a, head a : b)

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

