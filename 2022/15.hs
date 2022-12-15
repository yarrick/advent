import Data.Char
import Data.List

type Coord = (Int, Int)

dist :: (Coord, Coord) -> Int
dist ((xa,ya), (xb,yb)) = abs (xa - xb) + abs (ya - yb)

coverage :: Coord -> Int -> Int -> [(Int,Int)]
coverage (x,y) reach yrow
    | reach <= 0 = []
    | y == yrow = [(x-reach,x+reach)]
    | otherwise = coverage (x,yrow) (reach - ydiff) yrow
    where ydiff = abs (yrow - y)

addseg :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
addseg a [] = a
addseg [] b = b
addseg ((af,at):bs) ((nf,nt):ns)
    | nt < af = (nf,nt) : addseg ((af,at):bs) ns                    -- n fully before a
    | nf < af && nt >= af && nt <= at = addseg ((nf,at):bs) ns      -- n into a
    | nf >= af && nf <= at && nt > at = addseg bs ((af,nt):ns)      -- n out of a
    | nf >= af && nt <= at = addseg ((af,at):bs) ns                 -- n inside a
    | nf < af && nt > at = addseg bs ((nf,nt):ns)                   -- bigger than a
    | at < nf = (af,at) : addseg bs ((nf,nt):ns)                    -- a fully before n

exclude :: [(Int,Int)] -> Int -> Int
exclude [] _ = 0
exclude ((af,at):bs) pos
    | pos >= af && pos <= at = 1
    | otherwise = exclude bs pos

ycover sc yrow = foldl addseg [] $ map (scan) sc
    where scan (a,b) = coverage a (dist (a,b)) yrow

hidden sc = filter (\(y,r) -> length r > 1) $ map ycalc [0..4000000]
    where ycalc yrow = (yrow, ycover sc yrow)

process :: [(Coord,Coord)] -> [String]
process rows = map show [knownclear 2000000, (succ xa) * 4000000 + y2 ]
    where bots yrow = nub $ map (fst.snd) $ filter (\(_,(x,y)) -> y == yrow) rows
          len yrow = sum (map (\(f,t) -> t - f + 1) $ ycover rows yrow)
          knownclear yrow = len yrow - sum (map (exclude (ycover rows yrow)) $ bots yrow)
          (y2,((_,xa):_)) = head $ hidden rows

parse :: String -> (Coord, Coord)
parse s = ((read a, read b), (read c, read d))
    where keep c = isDigit c || c == '-'
          (a:b:c:d:[]) = filter (\n -> length n > 0) $ map (filter keep) $ words s

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map parse . lines)

