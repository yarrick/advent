import Asmbunny

run str = reg
  where (_,_,reg,_) =runVM $ newVM (readInstr $ words str) [0,0,0,0]

run2 str = reg
  where (_,_,reg,_) =runVM $ newVM (readInstr $ words str) [0,0,1,0]

