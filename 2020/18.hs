import Data.Char
import Data.List

data Exp = Num Integer | Plus | Mul | Paren [Exp] | PlusParen [Exp] deriving (Eq,Show)

parse :: Int -> String -> [(Int,Exp)]
parse _ [] = []
parse d (' ':ss) = parse d ss
parse d s@(a:bs)
    | isDigit a = (d,Num (read num)) : parse d (drop (length num) s)
    | a == '+' = (d,Plus) : parse d bs
    | a == '*' = (d,Mul) : parse d bs
    | a == '(' = parse (succ d) bs
    | a == ')' = parse (pred d) bs
    where num = takeWhile isDigit s

level :: Int -> [(Int,Exp)] -> [Exp]
level _ [] = []
level d ex
    | length same > 0 = (map snd same) ++ level d (drop (length same) ex)
    | length upper > 0 = Paren (level (succ d) upper) : level d (drop (length upper) ex)
    where same = takeWhile (\(a,b) -> a == d) ex
          upper = takeWhile (\(a,b) -> a > d) ex

eval :: [Exp] -> Exp
eval [Num a] = Num a
eval (Paren ex:cs) = eval (eval ex:cs)
eval (PlusParen ex:cs) = eval (eval ex:cs)
eval (Num a:b:Paren ex:cs) = eval (Num a:b:eval ex:cs)
eval (Num a:b:PlusParen ex:cs) = eval (Num a:b:eval ex:cs)
eval (Num a:Plus:Num b:cs) = eval (Num (a+b):cs)
eval (Num a:Mul:Num b:cs) = eval (Num (a*b):cs)

addParen :: [Exp] -> [Exp]
addParen [] = []
addParen [Num a] = [Num a]
addParen (Paren a:Plus:Paren b:cs) = addParen $ (PlusParen [Paren (addParen a),Plus,Paren (addParen b)]) : cs
addParen (Paren a:Plus:b:cs) = addParen $ (PlusParen [Paren (addParen a),Plus,b]) : cs
addParen (a:Plus:Paren b:cs) = addParen $ (PlusParen [a,Plus,Paren (addParen b)]) : cs
addParen (a:Plus:b:cs) = addParen $ (PlusParen [a,Plus,b]) : cs
addParen (Paren ex:cs) = (Paren (addParen ex)) : addParen cs
addParen (Num a:Mul:cs) = (Num a) : Mul : addParen cs
addParen (a:cs) = a : addParen cs

process :: [[Exp]] -> [String]
process maths = map (show.get.eval.intersperse Plus) [res, res2]
    where res = map eval maths
          res2 = map (eval.addParen) maths
          get (Num a) = a

main :: IO ()
main = interact (unlines . process . map (level 0. parse 0) . lines)
