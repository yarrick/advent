import Intcode

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

square :: Int -> [(Int,Int)]
square n = [ (x,y) | x <- [0..(n-1)], y <- [0..(n-1)] ]

checkpos :: [Integer] -> (Int,Int) -> Bool
checkpos mem (x,y) = (1==) $ head $ outdata $ exec state
  where state = newstate mem [toInteger x,toInteger y]

run bytes = length $ filter id $ map (checkpos mem) (square 50)
  where mem = parse bytes

-- part 2

rowstart :: [Integer] -> Int -> Int -> Int -> [Int]
rowstart mem r start end
  | start >= end = []
  | res = [start]
  | otherwise = rowstart mem r (start+1) end
  where res = checkpos mem (r,start)

rowend :: [Integer] -> Int -> Int -> Int -> [Int]
rowend mem r pos end
  | pos >= end = []
  | res == False = [(pos-1)]
  | otherwise = rowend mem r (pos+1) end
  where res = checkpos mem (r,pos)

beampos :: [(Int,Bool)] -> [Int]
beampos res = map fst $ filter (\(p,b) -> b) res

checkrow mem row startcol endcol n
  | beamstart == [] = checkrow mem (row+1) startcol endcol n
  | checkpos mem (row, (head beamstart + n - 1)) == False = checkrow mem (row+1) (head beamstart) (head beamstart) n
  | beamend == [] = checkrow mem (row+1) (head beamstart) (head beamstart) n
  | checkpos mem ((row + n - 1), beampos) = (row,beampos)
  | checkpos mem ((row + 40 - 1), beampos) == False = checkrow mem (row+100) (head beamstart) (head beamend) n
  | checkpos mem ((row + 80 - 1), beampos) == False = checkrow mem (row+30) (head beamstart) (head beamend) n
  | otherwise = checkrow mem (row+1) (head beamstart) (head beamend) n
  where beamstart = rowstart mem row startcol (startcol + 3*n)
        beamend = rowend mem row (endcol + (head beamstart) - startcol) (endcol + 5*n)
        beampos = (head beamend) - n + 1

run2 bytes = x*10000+y
  where (x,y) = checkrow (parse bytes) 0 0 0 100

process :: String -> [String]
process rows = [show $ run rows, show $ run2 rows]

main :: IO ()
main = interact (unlines . process)
