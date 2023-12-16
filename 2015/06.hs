import Data.Char as Char

splitString :: String -> [String]
splitString "" = []
splitString x = w : splitString xx
    where
        w = takeWhile Char.isDigit x
        xx = drop (1 + length w) x

parse :: String -> [Integer]
parse str = map read $ splitString str

data Mode = On | Off | Toggle deriving Show

data Command = Command {
    mode :: Mode,
    minX :: Integer,
    maxX :: Integer,
    minY :: Integer,
    maxY :: Integer
} deriving Show

data Light = Light {
    state :: Bool,
    x :: Integer,
    y :: Integer
} deriving Show

buildCommand :: Mode -> String -> String -> Command
buildCommand mode start stop = Command mode minx maxx miny maxy
    where
        minval = parse start
        maxval = parse stop
        minx = minval !! 0
        miny = minval !! 1
        maxx = maxval !! 0
        maxy = maxval !! 1

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands ("toggle":b:c:d:ee) = buildCommand Toggle b d : parseCommands ee
parseCommands ("turn":b:c:d:e:ff)
    | b == "on" = buildCommand On c e : parseCommands ff
    | b == "off" = buildCommand Off c e : parseCommands ff

flick :: Mode -> Bool -> Integer -> Integer -> Light
flick On _ x y = Light True x y
flick Off _ x y = Light False x y
flick Toggle True x y = Light False x y
flick Toggle False x y = Light True x y

execCommand :: Light -> Command -> Light
execCommand (Light state x y) (Command mode minx maxx miny maxy)
    | minx <= x && maxx >= x && miny <= y && maxy >= y = flick mode state x y
    | otherwise = Light state x y

isOn :: Light -> Bool
isOn (Light state _ _) = state

run str = length $ filter isOn lights
    where
        cmds = parseCommands $ words str
        lights = [ foldl execCommand (Light False x y) cmds | x <- [0..999], y <- [0..999]]

-- part 2


data Light2 = Light2 {
    brightness :: Integer,
    xx :: Integer,
    yy :: Integer
} deriving Show

dec :: Integer -> Integer
dec 0 = 0
dec 1 = 0
dec x = x - 1

flick2 :: Mode -> Integer -> Integer -> Integer -> Light2
flick2 On state x y = Light2 (state+1) x y
flick2 Off state x y = Light2 (dec state) x y
flick2 Toggle state x y = Light2 (state+2) x y

execCommand2 :: Light2 -> Command -> Light2
execCommand2 (Light2 state x y) (Command mode minx maxx miny maxy)
    | minx <= x && maxx >= x && miny <= y && maxy >= y = flick2 mode state x y
    | otherwise = Light2 state x y

getBrightness :: Light2 -> Integer
getBrightness (Light2 br _ _ ) = br

run2 str = sum $ map getBrightness lights
    where
        cmds = parseCommands $ words str
        lights = [ foldl execCommand2 (Light2 0 x y) cmds | x <- [0..999], y <- [0..999]]

process rows = [show $ run rows, show $ run2 rows]

main :: IO ()
main = interact (unlines . process)
