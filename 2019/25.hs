import Intcode
import System.IO
import System.Environment

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

-- created manually by exploring the paths and finding objects
-- collects all items and drops them next to door
prep :: State -> State
prep st = exec $ foldl inputstr st [
  "north",
  "take astronaut ice cream",
  "south",
  "west",
  "take mouse",
  "north",
  "take ornament",
  "west",
  "north",
  "take easter egg",
  "east",
  "take hypercube",
  "north",
  "east",
  "take prime number",
  "west",
  "south",
  "west",
  "north",
  "west",
  "north",
  "take wreath",
  "south",
  "east",
  "south",
  "south",
  "west",
  "take mug",
  "west"]

trydoor :: State -> [String] -> State
trydoor st drops = exec $ foldl inputstr (st {outdata=[]}) cmds
  where cmds = map ("drop "++) drops ++ ["north"]

combos :: [[String]] -> [String] -> [[String]]
combos a [] = a
combos prev (i:is) = combos (prev ++ (map (i:) prev)) is

gendrops :: State -> [String]
gendrops st = map (drop 2) $ filter (\s -> take 2 s == "- ") rows
  where rows = outputstr $ exec $ inputstr (st {outdata=[]}) "inv"

trydoors :: State -> State
trydoors st = head $ filter (\x -> not $ elem "Droids" $ concatMap words $ outputstr x) attempts
  where attempts = map (trydoor st) (combos [[]] (gendrops st))

repl :: State -> IO ()
repl st = do
  mapM putStrLn $ outputstr next
  instr <- getLine
  repl $ inputstr next { outdata = [] } instr
  return ()
  where next = exec st

-- run with file as argument to explore game
explore :: String -> IO ()
explore file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  repl $ newhaltstate (parse contents) [] needinput
  hClose handle

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  mapM putStrLn $ outputstr $ trydoors $ prep $ newhaltstate (parse contents) [] needinput
  hClose handle
  return ()
