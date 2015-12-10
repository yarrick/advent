
floors :: String -> Integer
floors directions = steps 0 directions

steps :: Integer -> String -> Integer
steps x (d:dd) = steps (stepFloor x d) dd
steps x _ = x

stepFloor :: Integer -> Char -> Integer
stepFloor x '(' = x+1
stepFloor x ')' = x-1
stepFloor x _ = x


--- Part two

basement :: String -> Integer
basement directions = bsteps 0 0 directions

bsteps :: Integer -> Integer -> String -> Integer
bsteps x s (d:dd) = baseStep (stepFloor x d) (s+1) dd
bsteps x s _ = -1

baseStep :: Integer -> Integer -> String -> Integer
baseStep x s dd
  | x == -1    = s
  | otherwise  =  bsteps x s dd
