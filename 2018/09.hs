import Control.DeepSeq

type Circle = ([Int],Int)

place :: Circle -> Int -> Circle
place (circle,curr) new
  | newpos == 0 = (circle ++ [new], length circle)
  | otherwise = deepseq newcircle (newcircle,newpos)
  where newpos = mod (curr + 2) (length circle)
        newcircle = (take newpos circle) ++ [new] ++ (drop newpos circle)

steal :: Circle -> (Int,Circle)
steal (circle,curr) = (circle!!stealpos,
  (take stealpos circle ++ drop (stealpos+1) circle, stealpos))
  where stealpos = mod (curr - 7) (length circle)

play :: (Circle,[Int]) -> Int -> (Circle,[Int])
play (circle, (score:ss)) marble
  | mod marble 23 == 0 = deepseq newscore (ncircle, newscore)
  | otherwise = deepseq movescore (place circle marble,movescore)
  where (stolen, ncircle) = steal circle
        marblescore = marble+stolen
        newscore = ss++[score+marblescore]
        movescore = ss++[score]

game :: Int -> (Circle,[Int]) -> Int -> (Circle,[Int])
game lastscore state marble
  | marble == lastscore = nstate
  | otherwise = game lastscore nstate (marble+1)
  where nstate = play state marble

run players lastscore = maximum playerscore
  where (circle, playerscore) = game lastscore (([0],0),take players $ repeat 0) 1
