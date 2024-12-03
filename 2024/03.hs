import Data.Char

data Token = Mul | LParen | RParen | Comma | Num Int | Do | Dont | Junk deriving (Eq, Show)

process :: [(Bool,[Int])] -> [String]
process ts = map (show.sum.map (product.snd)) [ts, filter fst ts]

extract :: Bool -> [Token] -> [(Bool,[Int])]
extract _ [] = []
extract en (Mul:LParen:Num a:Comma:Num b:RParen:ts) = (en,[a,b]) : extract en ts
extract en (Do:LParen:RParen:ts) = extract True ts
extract en (Dont:LParen:RParen:ts) = extract False ts
extract en (t:ts) = extract en ts

parse :: String -> [Token]
parse [] = []
parse ('(':ss) = LParen : parse ss
parse (')':ss) = RParen : parse ss
parse (',':ss) = Comma : parse ss
parse s
    | isDigit (head s) = Num (read $ takeWhile isDigit s) : parse (dropWhile isDigit s)
    | take 3 s == "mul" = Mul : parse (drop 3 s)
    | take 5 s == "don't" = Dont : parse (drop 5 s)
    | take 2 s == "do" = Do : parse (drop 2 s)
    | otherwise = Junk : parse (tail s)

main :: IO ()
main = interact (unlines . process . extract True . parse)
