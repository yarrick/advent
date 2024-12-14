import Data.Char

type Pos = [Int] -- X, Y

process ts = map (show.run) [ts, map adjust ts]
    where run ps = sum $ map (\(a,b) -> 3*a+b) $ concatMap play ps
          adjust (ab,p) = (ab,map (10000000000000+) p)

play ((a@(xa:ya:[]), b@(xb:yb:[])), (px:py:[]))
    | arest /= 0 || brest /= 0 = []
    | otherwise = [(a, b)]
    where (a,arest) = divMod (px*yb - xb*py) (xa*yb - ya*xb)
          (b,brest) = divMod (py-a*ya) yb

parse :: [String] -> [((Pos, Pos), Pos)]
parse [] = []
parse ("":ps) = parse ps
parse (a:b:p:ps) = ((get a 2, get b 2), get p 1) : parse ps
    where get s skips = map (read.filter isDigit) $ drop skips $ words s

main :: IO ()
main = interact (unlines . process . parse . lines)
