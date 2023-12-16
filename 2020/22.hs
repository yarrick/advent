import Data.Char
import qualified Data.Sequence as S
import Data.Foldable

parse :: [String] -> (S.Seq Int, S.Seq Int)
parse rows = (get first, get second)
    where first = takeWhile (/="") rows
          second = drop (length first+1) rows
          get n = S.fromList $ map read $ filter (all isDigit) n

play :: (S.Seq Int, S.Seq Int) -> (S.Seq Int, S.Seq Int)
play (a,b)
    | aa > ba = (as S.|> aa S.|> ba, bs)
    | otherwise = (as, bs S.|> ba S.|> aa)
    where draw s = (S.index s 0, S.drop 1 s)
          (aa, as) = draw a
          (ba, bs) = draw b

game :: (S.Seq Int, S.Seq Int) -> (S.Seq Int, S.Seq Int)
game (a,b)
    | null a || null b = (a,b)
    | otherwise = game $ play (a,b)

play2 :: (S.Seq Int, S.Seq Int) -> (S.Seq Int, S.Seq Int)
play2 (a,b)
    | recurse && null bres = (as S.|> aa S.|> ba, bs)
    | recurse && null ares = (as, bs  S.|> ba S.|> aa)
    | aa > ba = (as S.|> aa S.|> ba, bs)
    | otherwise = (as, bs S.|> ba S.|> aa)
    where draw s = (S.index s 0, S.drop 1 s)
          (aa, as) = draw a
          (ba, bs) = draw b
          recurse = S.length as >= aa && S.length bs >= ba
          (ares, bres) = game2 (S.take aa as,S.take ba bs) []

game2 :: (S.Seq Int, S.Seq Int) -> [(S.Seq Int, S.Seq Int)] -> (S.Seq Int, S.Seq Int)
game2 (a,b) hist
    | elem (a,b) hist = (a, S.empty)
    | null a || null b = (a,b)
    | otherwise = game2 (play2 (a,b)) ((a,b):hist)

score :: (S.Seq Int, S.Seq Int) -> Int
score (a,b)
    | null a = points (toList b) (scores b)
    | otherwise = points (toList a) (scores a)
    where scores ls = reverse [1..S.length ls]
          points [] [] = 0
          points (c:cs) (s:ss) = c*s + points cs ss

process :: (S.Seq Int, S.Seq Int) -> [String]
process rows = map (show.score) [game rows, game2 rows []]

main :: IO ()
main = interact (unlines . process . parse . lines)
