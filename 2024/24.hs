import qualified Data.Map as M
import Data.List

process (vals, ops) = [show $ val 1 zs]
    where zs = map snd $ sort $ filter (\(k,v) -> head k == 'z') $ M.toList $ deduce vals ops
          val n [] = 0
          val n (b:bs)
            | b = n + val (2*n) bs
            | otherwise = val (2*n) bs

deduce m [] = m
deduce m ops = deduce (M.insert out val m) (ps++later)
    where (((a,op,b,out):ps),later) = partition (\(a,_,b,_) -> (M.member a m) && (M.member b m)) ops
          val
            | op == "AND" = (m M.! a) && (m M.! b)
            | op == "OR" = (m M.! a) || (m M.! b)
            | (m M.! a) == (m M.! b) = False
            | otherwise = True

parse :: [String] -> (M.Map String Bool, [(String,String,String,String)])
parse ss = (M.fromList $ map (val.words) vals, map (func.words) outs)
    where (vals, (blank:outs)) = break (""==) ss
          val (v:d:[]) = (init v,d == "1")
          func (a:op:b:_:out:[]) = (a,op,b,out)

main :: IO ()
main = interact (unlines . process . parse . lines)
