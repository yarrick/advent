import Asmbunny

run str = runVM $ newVM (readInstr $ words str) [7,0,0,0]

