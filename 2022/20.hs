import Data.Maybe
import qualified Data.Sequence as S
import Control.DeepSeq

move :: S.Seq Int -> Int -> Int -> S.Seq Int
move seq _ 0 = seq
move seq pos moves
    | moves > lpos = move seq pos (mod moves lpos)
    | moves < -lpos = move seq pos (-(mod (abs moves) lpos))
    | moves < 0 && pos == 0 = move ((S.|>) (S.drop 1 seq) (S.index seq 0)) lpos moves
    | moves > 0 && pos == lpos = move ((S.<|) (S.index seq lpos) (S.take lpos seq)) 0 moves
    | moves > 0 = mv succ pred
    | otherwise = mv pred succ
    where lpos = S.length seq - 1
          k = S.index seq pos
          mv dp dm = move (S.insertAt (dp pos) k $ S.deleteAt pos seq) (dp pos) (dm moves)

mix :: S.Seq Int -> (Int, Int) -> S.Seq Int
mix seq (val,steps) = deepseq ns ns
    where pos = fromJust $ S.elemIndexL val seq
          ns = move seq pos steps

swap :: [Int] -> S.Seq Int -> S.Seq Int
swap steps seq = S.mapWithIndex (\_ v -> steps !! v) seq

score :: S.Seq Int -> Int
score seq = sum $ map (\p -> S.index seq $ (zpos+p) `mod` (S.length seq)) [1000, 2000, 3000]
    where zpos = fromJust $ S.elemIndexL 0 seq

process :: [Int] -> [String]
process steps =  map (show.score) [swap steps $ mixed steps seq, swap s2 megamix]
    where seq = S.fromList $ take (length steps) [0..]
          mixed st s = foldl mix s $ zip [0..] st
          s2 = map (*811589153) steps
          megamix = foldl (\s f -> f s) seq $ replicate 10 (mixed s2)

main :: IO ()
main = interact (unlines . process . map read . lines)
