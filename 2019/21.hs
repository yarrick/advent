import Intcode
import Data.Char

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

run bytes = last $ outdata $ exec state
  where state = newhaltstate (parse bytes) cmd needinput
        cmd = map (toInteger.ord) $ unlines $ [
          "NOT A J", -- jump if A is hole
          "NOT B T",
          "OR T J",  -- or if B is hole
          "NOT C T",
          "OR T J",  -- or if C is hole
          "AND D J", -- unless D is hole
          "WALK"]

run2 bytes = last $ outdata $ exec state
  where state = newhaltstate (parse bytes) cmd needinput
        cmd = map (toInteger.ord) $ unlines $ [
          "NOT B J", -- jump if B is hole
          "NOT C T",
          "OR T J",  -- or if C is hole
          "AND D J", -- unless D is hole
          "AND H J", -- unless H is hole
          "NOT A T",
          "OR T J",  -- or if A is hole
          "RUN"]

