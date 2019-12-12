import Data.List
import Intcode

parse :: String -> [Integer]
parse [] = []
parse w = read num : parse (drop 1 end)
    where (num, end) = break (','==) w

amp :: [Integer] -> Integer -> Integer -> Integer
amp mem index input = head out
  where (_,_,_,out,_,_) = exec $ newstate mem [index, input]

amps :: [Integer] -> Integer -> [Integer] -> Integer
amps _ v [] = v
amps mem val (s:ss) = amps mem out ss
  where out = amp mem s val

permute :: [([Integer],[Integer])] -> [[Integer]]
permute ps@((a,[]):as) = map fst ps
permute ps = permute $ concat $ map generate ps

generate :: ([Integer],[Integer]) -> [([Integer],[Integer])]
generate (a,b) = [ (a ++ [g], bs) | (g,bs) <- grab b]

grab :: [Integer] -> [(Integer,[Integer])]
grab g = [ (a, filter (a/=) bs) | (a, bs) <- opts]
  where opts = zip g (take (length g) $ repeat g)

run bytes = maximum $ map (amps mem 0) $ permute [([],[0,1,2,3,4])]
  where mem = parse bytes

-- part 2

runamp :: State -> State
runamp d@(pc,mem,inp,_,b,h) = exec (pc,mem,inp,[],b,h)

newamp :: [Integer] -> State
newamp mem = newhaltstate mem [] (\out -> length out > 0)

ampinput :: (State, [Integer]) -> State
ampinput (state,i) = foldl inputnum state i

-- feed int into first amp in list
feedamp :: [State] -> [Integer] -> [State]
feedamp [] _ = []
feedamp (a:as) val = ampinput (a,val) : as

newamps :: [Integer] -> [Integer] -> [State]
newamps mem settings = feedamp setupamps [0]
  where
    setupamps = map ampinput (zip amps $ group settings)
    amps = take (length settings) (repeat $ newamp mem)

runamps :: [State] -> [State]
runamps [] = []
runamps (a:as) = o : (runamps $ feedamp as out)
  where o@(_,_,_,out,_,_) = runamp a

loopamps :: [State] -> Integer
loopamps amps@((_,_,res,_,_,_):as)
  | pc < 0 = head res
  | otherwise = loopamps $ runamps $ feedamp amps lastout
  where (pc,_,_,lastout,_,_) = last amps

test2 bytes settings = loopamps $ newamps mem settings
  where mem = parse bytes

run2 bytes = maximum $ map loopamps $ map (newamps mem) $ permute [([],[5,6,7,8,9])]
  where mem = parse bytes
