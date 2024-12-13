import Data.Char

type Pos = [Int] -- X, Y

process ts = [show $ 3*sum as + sum bs]
    where (as,bs) = unzip $ concatMap play ts

play ((a, b), goal)
    | bfirst /= [] = bfirst
    | otherwise = map (\(a,b) -> (b,a)) $ push ((b,a),goal) 0 (steps a)
    where steps n = minimum $ map succ $ zipWith div goal n
          bfirst = push ((a,b),goal) 0 (steps b)

push ((a@(ax:ay:[]), b@(bx:by:[])), goal) as bs
    | goal == [x,y] = [(as,bs)]
    | as < 0 || bs < 0 = []
    | head goal < x && bx > ax = push ((a,b),goal) as (pred bs)
    | last goal < y && by > ay = push ((a,b),goal) as (pred bs)
    | x > (head goal + ax + bx) || y > (last goal + ay + by) = []
    | otherwise = push ((a,b),goal) (succ as) bs
    where x = as*ax + bs*bx
          y = as*ay + bs*by

parse :: [String] -> [((Pos, Pos), Pos)]
parse [] = []
parse ("":ps) = parse ps
parse (a:b:p:ps) = ((get a 2, get b 2), get p 1) : parse ps
    where get s skips = map (read.filter isDigit) $ drop skips $ words s

main :: IO ()
main = interact (unlines . process . parse . lines)
