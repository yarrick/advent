import Data.Char
import Data.List

-- divisor, dst true, dst other, next val func
type Monkey = (Int, Int, Int, Int -> Int)

handle :: Int -> Monkey -> Int -> (Int, Int)
handle dvd (dv,td,fd,nv) worry
    | mod nworry dv == 0 = (td, nworry)
    | otherwise = (fd, nworry)
    where nworry = div (nv worry) dvd

catch :: [([Int], Monkey, Int)] -> Int -> Int -> [(Int, Int)] -> [([Int], Monkey, Int)]
catch [] _ _ _ = []
catch ((it,mk,count):mks) num active midair
    | num == active = ([],mk,count+length midair) : catch mks (succ num) active midair
    | otherwise = (it ++ (map snd $ filter (\(dst,_) -> dst == num) midair),mk,count)
                  : catch mks (succ num) active midair

doround :: Int -> Int -> [([Int], Monkey, Int)] -> [([Int], Monkey, Int)]
doround pos dvd mks
    | pos < length mks = doround (succ pos) dvd (catch mks 0 pos thrown)
    | otherwise = mks
    where (it,active,_) = mks !! pos
          thrown = map (handle dvd active) it

process :: [([Int], Monkey, Int)] -> [String]
process mks = map (show.business) [rounds 3 20, rounds 1 10000]
    where rounds dvd rnds = map (\(_,_,m) -> m) $ (iterate (doround 0 dvd) mks) !! rnds
          business n = product $ take 2 $ reverse $ sort n

parse :: [String] -> [([Int], Monkey, Int)]
parse [] = []
parse rows = (items, (lastint 3, lastint 4, lastint 5,
              (\n -> (op (expr !! 1)) [(opr (expr !! 0)) n, (opr (expr !! 2)) n])),0)
              : parse (drop (length chunk +1) rows)
    where chunk = takeWhile (/= "") rows
          items = map (read.filter isDigit) $ words $ drop 18 (chunk !! 1)
          lastint row = read $ last $ words (chunk !! row)
          opr "old" = id
          opr num = (\_ -> (read num))
          expr = words $ drop 18 (chunk !! 2)
          op "+" = sum
          op "*" = product


-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)

