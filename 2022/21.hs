import Data.Char
import Data.List
import qualified Data.Map as M

data Expr = Num Int | Op Expr String (Int->Int->Int) Expr | Human
instance Show (Expr) where
    show (Num v) = show v
    show (Op le s op re) = "(" ++ show le ++ ") " ++ s ++ " (" ++ show re ++ ")"
    show (Human) = "Human"

build :: Bool -> M.Map String [String] -> String -> Expr
build humn mm tag
    | humn && tag == "humn" = Human
    | length ls == 1 = Num (read $ head ls)
    | op == "+" = Op lexpr op (+) rexpr
    | op == "-" = Op lexpr op (-) rexpr
    | op == "*" = Op lexpr op (*) rexpr
    | op == "/" = Op lexpr op (div) rexpr
    where ls = (M.!) mm tag
          op = ls !! 1
          lexpr = build humn mm (ls !! 0)
          rexpr = build humn mm (ls !! 2)

eval :: Expr -> Int
eval (Num v) = v
eval (Op le _ op re) = op (eval le) (eval re)

get :: Expr -> [Expr]
get (Num v) = [Num v]
get (Op le _ _ re) = [le,re]

humanexpr :: Expr -> Bool
humanexpr Human = True
humanexpr (Num _) = False
humanexpr (Op le _ _ re) = humanexpr le || humanexpr re

invop :: Expr -> (Int->Int->Int)
invop (Op _ "+" _ _) = (-)
invop (Op _ "-" _ _) = (+)
invop (Op _ "*" _ _) = (div)
invop (Op _ "/" _ _) = (*)

deduce target Human = target
deduce target e@(Op le s op re)
    | humanexpr le = deduce (iop target (eval re)) le
    | s == "/" = deduce (eval le) (Op (Num target) "*" (*) re)
    | s == "-" = deduce (eval le) (Op (Num target) "+" (+) re)
    | otherwise = deduce (iop target (eval le)) re
    where iop = invop e

process :: M.Map String [String] -> [String]
process mm = map show [eval $ build False mm "root", deduce (eval fside) hside]
    where ((hside:[]),(fside:[])) = partition humanexpr $ get $ build True mm "root"

parse :: String -> (String,[String])
parse ss = (filter isAlpha $ head ws, tail ws)
    where ws = words ss

main :: IO ()
main = interact (unlines . process . M.fromList . map parse . lines)
