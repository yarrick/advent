import qualified Data.Sequence as S
import Data.List

steps :: S.Seq Int -> Int -> Int -> (Int -> Int) -> Int
steps jumps count pos fn
    | pos >= S.length jumps = count
    | otherwise = steps nextjumps (count+1) (pos+val) fn
    where val = S.index jumps pos
          nextjumps = S.adjust fn pos jumps

process :: [String] -> [String]
process rows = map (show.steps nums 0 0) [succ, nextjump]
    where nums = S.fromList $ map read rows
          nextjump x
            | x >= 3 = x-1
            | otherwise = x+1

main :: IO ()
main = interact (unlines . process . lines)
