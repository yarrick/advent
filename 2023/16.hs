import Prelude hiding (Left, Right)
import qualified Data.Map as M

data Dir = Up | Right | Down | Left deriving (Enum, Eq, Show)
type Pos = (Int,Int)

process :: [String] -> [String]
process rows = map show [beamed ((1,0),Right), maximum $ map beamed edges ]
    where rcells (r,row) = [((r,c),s) | (c,s) <- zip [1..] row ]
          cells = M.fromList $ concatMap rcells $ zip [1..] rows
          beamed b = length $ M.toList $ beam cells M.empty [b]
          horiz r d = [ ((r,c),d) | c <- [1..length (head rows)] ]
          vert c d = [ ((r,c),d) | r <- [1..length rows] ]
          edges = horiz 0 Down ++ vert 0 Right ++
                  horiz (1+length rows) Up ++ vert (1+length (head rows)) Left

beam :: M.Map Pos Char -> M.Map Pos [Dir] -> [(Pos, Dir)] -> M.Map Pos [Dir]
beam _ a [] = a
beam cells hist ((pos, dir):ps)
    | M.notMember npos cells = beam cells hist ps
    | elem dir (M.findWithDefault [] npos hist) = beam cells hist ps
    | char == '.' = beam cells logged ((npos,dir):ps)
    | char == '|' && elem dir [Up, Down] = beam cells logged ((npos,dir):ps)
    | char == '|' = beam cells logged ((npos,Up):(npos,Down):ps)
    | char == '-' && elem dir [Left, Right] = beam cells logged ((npos,dir):ps)
    | char == '-' = beam cells logged ((npos,Left):(npos,Right):ps)
    | char == '/' = beam cells logged ((npos,bounceSlash dir):ps)
    | char == '\\' = beam cells logged ((npos,bounceBackslash dir):ps)
    where npos = next pos dir
          char = cells M.! npos
          logged = M.insertWith (++) npos [dir] hist
          bounceSlash Right = Up
          bounceSlash Left = Down
          bounceSlash dir = succ dir
          bounceBackslash Up = Left
          bounceBackslash Right = Down
          bounceBackslash Down = Right
          bounceBackslash Left = Up

next :: (Int, Int) -> Dir -> (Int, Int)
next (r,c) Up = (r-1,c)
next (r,c) Down = (r+1,c)
next (r,c) Left = (r,c-1)
next (r,c) Right = (r,c+1)

main :: IO ()
main = interact (unlines . process . lines)
