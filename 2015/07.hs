import Data.Bits hiding (And)
import Data.Char (isDigit)
import Data.Maybe

data Arg = Ref String | Val Int deriving (Show, Read)

data Op = Not Arg | And Arg Arg |
    Or Arg Arg | LShift Arg Int |
    RShift Arg Int | Assign String | Value Int deriving (Show, Read)

getArg :: String -> Arg
getArg str@(a:aa)
    | isDigit a = Val (read str)
    | otherwise = Ref str

parse :: [String] -> [(String,Op)]
parse [] = []
parse (arg1:"AND":arg2:_:dest:ss) = (dest, And (getArg arg1) (getArg arg2)) : parse ss
parse (arg1:"OR":arg2:_:dest:ss) = (dest, Or (getArg arg1) (getArg arg2)) : parse ss
parse (arg1:"LSHIFT":arg2:_:dest:ss) = (dest, (LShift (getArg arg1) (read arg2))) : parse ss
parse (arg1:"RSHIFT":arg2:_:dest:ss) = (dest, (RShift (getArg arg1) (read arg2))) : parse ss
parse ("NOT":arg1:_:dest:ss) = (dest, Not (getArg arg1)) : parse ss
parse (arg1@(a:aa):"->":dest:ss)
    | isDigit a = (dest, (Value $ read arg1)) : parse ss
    | otherwise = (dest, (Assign arg1)) : parse ss


get :: Maybe Op -> Maybe Int
get (Just (Value x)) = Just x
get _ = Nothing

getval :: Arg -> [(String,Op)] -> Maybe Int
getval (Val num) _ = Just num
getval (Ref str) wires = get $ lookup str wires

doNot :: (String,Op) -> Maybe Int -> (String,Op)
doNot w Nothing = w
doNot (name, _) (Just x) = (name, Value ((complement x) .&. 0xffff))

doAnd :: (String,Op) -> Maybe Int -> Maybe Int -> (String,Op)
doAnd (name, _) (Just x) (Just y) = (name, Value (x .&. y))
doAnd w Nothing _ = w
doAnd w _ Nothing = w

doOr :: (String,Op) -> Maybe Int -> Maybe Int -> (String,Op)
doOr (name, _) (Just x) (Just y) = (name, Value (x .|. y))
doOr w Nothing _ = w
doOr w _ Nothing = w

doLShift :: (String,Op) -> Maybe Int -> (String,Op)
doLShift w Nothing = w
doLShift (name, LShift _ y) (Just x) = (name, Value ((shift x y) .&. 0xffff))

doRShift :: (String,Op) -> Maybe Int -> (String,Op)
doRShift w Nothing = w
doRShift (name, RShift _ y) (Just x) = (name, Value ((shift x (-y)) .&. 0xffff))

doAssign :: (String,Op) -> Maybe Int -> (String,Op)
doAssign w Nothing = w
doAssign (name, Assign _) (Just x) = (name, Value x)

calc :: (String,Op) -> [(String,Op)] -> (String,Op)
calc ww@(_, (Not w)) wires = doNot ww (getval w wires)
calc ww@(_, (And x y)) wires = doAnd ww (getval x wires) (getval y wires)
calc ww@(_, (Or x y)) wires = doOr ww (getval x wires) (getval y wires)
calc ww@(_, (LShift reg x)) wires = doLShift ww (getval reg wires)
calc ww@(_, (RShift reg x)) wires = doRShift ww (getval reg wires)
calc ww@(_, (Assign reg)) wires = doAssign ww (getval (Ref reg) wires)

-- full list as second arg
work :: [(String,Op)] -> [(String,Op)] -> [(String,Op)]
work [] _ = []
work (w@(_, (Value _)):ww) wires = w : work ww wires
work (w:ww) wires = calc w wires : work ww wires

solve :: [(String,Op)] -> [(String,Op)]
solve ops = work ops ops

-- run 5000 iterations of solving, should be enough
run str = fromJust $ getval (Ref "a") $ head $ drop 5000 $ iterate solve ops
    where ops = parse $ words str

-- part 2 by just changing inputfile (x -> b), set x to answer from part 1
process rows = [show $ run rows]

main :: IO ()
main = interact (unlines . process)
