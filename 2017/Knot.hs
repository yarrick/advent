
module Knot where

import Data.Char
import Data.Bits
import Text.Printf

twist :: [Int] -> Int -> [Int]
twist ts n = (reverse $ take n ts) ++ drop n ts

move :: [Int] -> Int -> [Int]
move ts n = take (length ts) $ drop n $ cycle ts

step :: ([Int],Int) -> (Int,Int) -> ([Int],Int)
step (ts,moves) (pos,skip) = (move (twist ts pos) offset, moves+offset)
    where offset = (pos+skip)

run :: [Int] -> [Int] -> [Int]
run list twists = move knotted extramoves -- restore starting pos
    where (knotted,moves) = foldl step (list,0) $ zip twists [0..]
          extramoves = length list - mod moves (length list)

hexed :: [Int] -> String
hexed [] = []
hexed nums = printf "%02x" val ++ hexed (drop 16 nums)
    where val = foldl1 xor (take 16 nums)

knot str = hexed $ run [0..255] twists
    where input = map ord str ++ [17, 31, 73, 47, 23]
          twists = take (64 * length input) $ cycle input

