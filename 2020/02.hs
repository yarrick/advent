import Data.List
import Data.Char

chars :: [Char] -> [(Char,Int)]
chars [] = []
chars str = map glen gs
    where gs = group $ sort str
          glen g = (head g, length g)

parse :: String -> ((Char,Int,Int),[(Char,Int)])
parse str = ((head c,read fnum,read lnum), chars pass)
    where (num:c:pass:end) = words str
          nums = groupBy (\ _ x -> isDigit x) num
          fnum = head nums
          lnum = tail $ head $ tail nums

parse2 :: String -> ((Char,Int,Int),String)
parse2 str = ((head c,read fnum,read lnum), pass)
    where (num:c:pass:end) = words str
          nums = groupBy (\ _ x -> isDigit x) num
          fnum = head nums
          lnum = tail $ head $ tail nums

isValid :: ((Char,Int,Int),[(Char,Int)]) -> Bool
isValid (_, []) = False
isValid ((c,lo,hi), ((pc, len):ps))
    | c == pc && len >= lo && len <= hi = True
    | c == pc = False
    | otherwise = isValid ((c,lo,hi), ps)

isValid2 :: ((Char,Int,Int), String) -> Bool
isValid2 ((c,lo,hi), pass)
    | lomatch && himatch = False
    | lomatch || himatch = True
    | otherwise = False
        where lomatch = c == pass !! (lo-1)
              himatch = c == pass !! (hi-1)

process :: [String] -> [String]
process rows = map (show . length . filter id) [map isValid passes,
                                                map isValid2 passes2]
    where passes = map parse rows
          passes2 = map parse2 rows

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . lines)

