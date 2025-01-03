import Control.DeepSeq
import qualified Data.IntMap.Strict as M

-- value to write, ptr offset, next state
type StateOp = (Int, Int, Char)
type State = (Char, [StateOp])

-- state, steps, mem, memptr, states
type Machine = (Char,Int,M.IntMap Int,Int,[State])

step :: Machine -> Machine
step m@(_,0,_,_,_) = m
step (st,steps,mem,ptr,states) = step $ deepseq next next
    where state = fetch states st
          val = M.findWithDefault 0 ptr mem
          (wval,offs,nst) = (snd state) !! val
          next = (nst,pred steps,M.insert ptr wval mem,ptr+offs,states)

fetch :: [State] -> Char -> State
fetch sts c = head $ filter (\s -> fst s == c) sts

parse :: [String] -> (Char, Int, [State])
parse (b:s:ls) = (begin, initlen, readStates ls)
    where begin = head $ (words b) !! 3
          initlen = read $ (words s) !! 5

readStates :: [String] -> [State]
readStates [] = []
readStates (_:sn:def) = (name,defs): readStates (drop 8 def)
    where name = head $ (words sn) !! 2
          defs = readDef (take 8 def)

readDef :: [String] -> [StateOp]
readDef [] = []
readDef (cur:wr:mov:cont:ndef) = (wval,ptrdir,nst) : readDef ndef
    where wval = read $ take 1 $ (words wr) !! 4
          ptrdir
            | last (words mov) == "right." = 1
            | otherwise = -1
          nst = head $ (words cont) !! 4

process :: (Char, Int, [State]) -> [String]
process (start,steps,ss) = map show [sum $ M.elems mm]
    where mach = (start,steps,M.empty,5000,ss)
          (_,_,mm,_,_) = step mach

main :: IO ()
main = interact (unlines . process . parse . lines)
