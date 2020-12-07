import qualified Data.Sequence as S

type Circle = (S.Seq Int,Int)

place :: Circle -> Int -> Circle
place (circle,curr) new
  | newpos == 0 = (circle S.|> new, S.length circle)
  | otherwise = (newcircle,newpos)
  where newpos = mod (curr + 2) (S.length circle)
        newcircle = S.insertAt newpos new circle

steal :: Circle -> (Int,Circle)
steal (circle,curr) = (S.index circle stealpos, (S.deleteAt stealpos circle,stealpos))
  where stealpos = mod (curr - 7) (S.length circle)

play :: (Circle,[Int]) -> Int -> (Circle,[Int])
play (circle, (score:ss)) marble
  | mod marble 23 == 0 = (ncircle, newscore)
  | otherwise = (place circle marble,movescore)
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
  where (circle, playerscore) = game lastscore ((S.singleton 0,0),replicate players 0) 1
