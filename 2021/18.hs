import Data.Char
import Control.Parallel

data Sn = Num Int | Pair Sn Sn deriving (Eq)
data Dir = L | R deriving (Eq,Show)

instance Show Sn where
    show (Num a) = show a
    show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

parse :: [Sn] -> String -> ([Sn], String)
parse sn [] = (sn,[])
parse sn ('[':ns) = parse (sn ++ [Pair (head inner) (last inner)]) rest
    where (inner,rest) = parse [] ns
parse sn (']':rest) = (sn,rest)
parse sn (cs)
    | isDigit (head cs) = parse (sn ++ [Num (read num)]) end
    | head cs == ',' = parse sn (tail cs)
    where (num,end) = break (not.isDigit) cs

exploders :: [Dir] -> Sn -> [([Dir],Int,Int)]
exploders _ (Num a) = []
exploders ds (Pair a b)
    | length ds < 4 = (exploders (ds++[L]) a) ++ (exploders (ds++[R]) b)
    | otherwise = [(ds,num a, num b)]
        where num (Num n) = n

go :: Dir -> Sn -> [Dir] -> [Dir]
go defdir (Pair a b) (L:ds) = L : go defdir a ds
go defdir (Pair a b) (R:ds) = R : go defdir b ds
go L (Pair a b) [] = L : go L a []
go R (Pair a b) [] = R : go R b []
go _ (Num _) _ = []

sideOf :: (Dir,Dir) -> [Dir] -> Sn -> [Dir]
sideOf (undo,defdir) ds t
    | firstturn == [] = []
    | otherwise = go defdir t (init firstturn ++ [undo])
    where firstturn = reverse $ dropWhile (==undo) $ reverse ds

leftOf :: [Dir] -> Sn -> [Dir]
leftOf = sideOf (L, R)

rightOf :: [Dir] -> Sn -> [Dir]
rightOf = sideOf (R, L)

update :: Sn -> [Dir] -> [([Dir],(Sn -> Sn))] -> Sn
update (Num a) path ops
    | length thisop > 0 = (snd $ head thisop) (Num a)
    | otherwise = Num a
    where thisop = filter (\(p,_) -> length p > 0 && p == path) ops
update (Pair a b) path ops
    | length thisop > 0 = (snd $ head thisop) (Pair a b)
    | otherwise = Pair (update a (path++[L]) ops) (update b (path++[R]) ops)
    where thisop = filter (\(p,_) -> length p > 0 && p == path) ops

explode :: Sn -> ([Dir],Int,Int) -> Sn
explode n (target,lval,rval) = update n []
    [(leftOf target n,adder lval),(target,setter 0),(rightOf target n,adder rval)]
    where adder v (Num a) = Num (a+v)
          setter v _ = Num v

splits :: [Dir] -> Sn -> [([Dir],Int,Int)]
splits ds (Num a)
    | a >= 10 && a == half + half = [(ds,half,half)]
    | a >= 10 = [(ds, half,half+1)]
    | otherwise = []
    where half = div a 2
splits ds (Pair a b) = (splits (ds++[L]) a) ++ (splits (ds++[R]) b)

dosplit :: Sn -> ([Dir],Int,Int) -> Sn
dosplit n (target,lval,rval) = update n [] [(target,\a -> (Pair (Num lval) (Num rval)))]

reduce :: Sn -> Sn
reduce n
    | length exp > 0 = reduce $ explode n (head exp)
    | length sp > 0 = reduce $ dosplit n (head sp)
    | otherwise = n
    where exp = exploders [] n
          sp = splits [] n

add :: Sn -> Sn -> Sn
add a b = reduce $ Pair a b

magnitude :: Sn -> Int
magnitude (Num n) = n
magnitude (Pair a b) = 3*(magnitude a) + 2*(magnitude b)

process :: [Sn] -> [String]
process nums = map show [magnitude summed, maximum $ psum pairs]
    where summed = foldl1 add nums
          pairs = [ (a,b) | a <- nums, b <- nums, a /= b]
          psum [] = []
          psum ((a,b):cs) = s `par` s : psum cs
            where s = magnitude $ add a b

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . (map (head.fst.parse [])) . lines)

