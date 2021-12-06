import Data.Matrix
import Control.DeepSeq

-- value to write, ptr offset, next state
type StateOp = (Int, Int, Char)
type State = (Char, [StateOp])

-- state, steps, mem, memptr, states
type Machine = (Char,Int,Matrix Int,Int,[State])

step :: Machine -> Machine
step m@(_,0,_,_,_) = m
step (st,steps,mem,ptr,states) = step $ deepseq next next
    where state = fetch states st
          val = getElem 1 ptr mem
          (wval,offs,nst) = (snd state) !! val
          next = (nst,pred steps,setElem wval (1,ptr) mem,ptr+offs,states)

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
process (start,steps,ss) = map show [sum $ toList mm]
    where mem = zero 1 20000
          mach = (start,steps,mem,5000,ss)
          (_,_,mm,_,_) = step mach

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
