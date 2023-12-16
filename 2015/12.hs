import Data.Char (isDigit, isLetter)

data Tok = ListStart | ListEnd | ObjStart | ObjEnd | Comma | Colon | TString String | TInt Int
    deriving Show

isNotNum :: Char -> Bool
isNotNum c = not $ isDigit c || c == '-'

isNotStr :: Char -> Bool
isNotStr c = not $ isLetter c || c == '"'

tokenize :: String -> [Tok]
tokenize [] = []
tokenize ('[':str) = ListStart : tokenize str
tokenize (']':str) = ListEnd : tokenize str
tokenize ('{':str) = ObjStart : tokenize str
tokenize ('}':str) = ObjEnd : tokenize str
tokenize (',':str) = Comma : tokenize str
tokenize (':':str) = Colon : tokenize str
tokenize str@('\"':_) = (TString word) : tokenize end
    where (word, end) = break isNotStr str
tokenize str = (TInt $ read num) : tokenize end
    where (num, end) = break isNotNum str

data Js = JsList [Js] | JsObj [Js] | JsInt Int | JsString String deriving Show

-- these can be simplified..
parseList :: [Tok] -> [Js] -> (Js, [Tok])
parseList (ListEnd:tokens) js = (JsList js, tokens)
parseList (TString str:tokens) js = parseList tokens (js ++ [JsString str])
parseList (TInt num:tokens) js = parseList tokens (js ++ [JsInt num])
parseList (Comma:tokens) js = parseList tokens js
parseList (ListStart:listtok) js = parseList tokens (js ++ [sublist])
    where (sublist, tokens) = parseList listtok []
parseList (ObjStart:objtok) js = parseList tokens (js ++ [obj])
    where (obj, tokens) = parseObj objtok []

parseObj :: [Tok] -> [Js] -> (Js, [Tok])
parseObj (ObjEnd:tokens) js = (JsObj js, tokens)
parseObj (TString str:tokens) js = parseObj tokens (js ++ [JsString str])
parseObj (TInt num:tokens) js = parseObj tokens (js ++ [JsInt num])
parseObj (Comma:tokens) js = parseObj tokens js
parseObj (Colon:tokens) js = parseObj tokens js
parseObj (ListStart:listtok) js = parseObj tokens (js ++ [sublist])
    where (sublist, tokens) = parseList listtok []
parseObj (ObjStart:objtok) js = parseObj tokens (js ++ [obj])
    where (obj, tokens) = parseObj objtok []

parse :: [Tok] -> Js
parse (ListStart:listtok) = list
    where (list, tokens) = parseList listtok []
parse (ObjStart:objtok) = obj
    where (obj, tokens) = parseObj objtok []

sumJs :: Int -> Js -> Int
sumJs x (JsInt y) = x + y
sumJs x (JsString _) = x
sumJs x (JsList l) = x + foldl sumJs 0 l
sumJs x (JsObj o) = x + foldl sumJs 0 o

--part 2
isred :: Js -> Bool
isred (JsString "\"red\"") = True
isred _ = False

nored :: Js -> Js
nored val@(JsInt i) = val
nored val@(JsString s) = val
nored (JsList list) = JsList (map nored list)
nored (JsObj objs)
    | any isred objs = JsObj []
    | otherwise = JsObj (map nored objs)

process row = map (show.sumJs 0) [parsed, nored parsed]
    where parsed = parse $ tokenize row

main :: IO ()
main = interact (unlines . process)
