data Instr = Noop | Addx Int deriving (Eq, Show)

run :: [Instr] -> (Int,Int) -> [(Int,Int)]
run [] a = []
run (Noop:is) (cyc,xval) = (succ cyc,xval) : run is (succ cyc,xval)
run ((Addx val):is) (cyc,xval) = (cyc+1,xval) : (cyc+2,xval) : run is (cyc+2,xval+val)

sprite :: (Int, (Int,Int)) -> Char
sprite (cyc, (_,x))
    | cyc >= x && cyc < (x+3) = '#'
    | otherwise = ' '

draw :: [(Int,Int)] -> [String]
draw [] = []
draw cs = (map sprite (zip [1..] $ take 40 cs)) : draw (drop 40 cs)

process :: [Instr] -> [String]
process rows = [show $ sum $ map (\(a,b) -> a*b) rssi] ++ draw res
    where res = run rows (0,1)
          rssi = filter (\(s,_) -> elem s [20, 60, 100, 140, 180, 220]) res

parse :: String -> Instr
parse "noop" = Noop
parse ('a':'d':'d':'x':xs) = Addx (read xs)

main :: IO ()
main = interact (unlines . process . map parse . lines)
