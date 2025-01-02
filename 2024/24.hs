import qualified Data.Map as M
import Data.List

process (vals, ops) = [show $ val 1 zs, concat $ intersperse "," swaps]
    where zs = map snd $ sort $ filter (\(k,v) -> head k == 'z') $ M.toList $ compute vals ops
          val n [] = 0
          val n (b:bs)
            | b = n + val (2*n) bs
            | otherwise = val (2*n) bs
          om = opmap M.empty ops
          swaps = sort $ adder vals om [] 1 $ fst $ getop om "x00" "AND" "y00"

opmap m [] = m
opmap m (o@(a,op,b,out):ops) = opmap (M.insert (a,op) o $ M.insert (b,op) o m) ops

compute m [] = m
compute m ops = compute (M.insert out val m) (ps++later)
    where (((a,op,b,out):ps),later) = partition (\(a,_,b,_) -> (M.member a m) && (M.member b m)) ops
          val
            | op == "AND" = (m M.! a) && (m M.! b)
            | op == "OR" = (m M.! a) || (m M.! b)
            | (m M.! a) == (m M.! b) = False
            | otherwise = True

getop om a op b
    | M.member (a,op) om && M.member (b,op) om = (aout, [])
    | M.member (a,op) om = (aout, swaps a [aa,b,ab])
    | M.member (b,op) om = (bout, swaps b [ba,a,bb])
    where (aa,_,ab,aout) = om M.! (a,op)
          (ba,_,bb,bout) = om M.! (b,op)
          swaps n cands = filter (n/=) cands

adder m ops swaps n carry
    | length ops < 100 = []
    | M.notMember x m = swaps
    | otherwise = adder m ops (swaps ++ (nub $ concat [s1,s2,s3,s4,s5])) (succ n) ncarry
    where [x,y] = [ c : (reverse $ take 2 $ reverse $ '0' : (show n)) | c <- "xy"]
          (xored,s1) = getop ops x "XOR" y
          (output,s2) = getop ops carry "XOR" xored
          (hcarry1,s3) = getop ops x "AND" y
          (hcarry2,s4) = getop ops xored "AND" carry
          (ncarry,s5) = getop ops hcarry1 "OR" hcarry2

parse :: [String] -> (M.Map String Bool, [(String,String,String,String)])
parse ss = (M.fromList $ map (val.words) vals, map (func.words) outs)
    where (vals, (blank:outs)) = break (""==) ss
          val (v:d:[]) = (init v,d == "1")
          func (a:op:b:_:out:[]) = (a,op,b,out)

main :: IO ()
main = interact (unlines . process . parse . lines)
