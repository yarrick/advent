import Intcode
import Data.List

type Packet = (Integer, [Integer])

needinput :: State -> Bool
needinput st
  | length (indata st) == 0 && mod nextop 100 == 3 = True
  | otherwise = False
  where nextop = (memory st) !! (fromInteger $ pc st)

feed :: State -> State
feed state
  | needinput state = inputnum state (-1)
  | otherwise = state

ticker :: [State] -> [State]
ticker [] = []
ticker (s:[]) = [s] -- NAT buffer
ticker (s:ss) = (step $ feed s) : ticker ss

splitpacket state
  | length (outdata state) == 3 = (state { outdata = [] }, outdata state)
  | otherwise = (state,[])

addpacket :: State -> [Integer] -> State
addpacket st bytes = foldl inputnum st bytes

sendpackets :: ([State],[Packet]) -> Integer -> [[Integer]] -> ([State],[Packet])
sendpackets (ss,pktlog) _ [] = (ss, pktlog)
sendpackets ((s:[]),pktlog) 50 (p:ps) = ([(addpacket s p)], pktlog ++ [(head p, tail p)])
sendpackets ((s:ss),pktlog) idx pkts@((dst:ds):ps)
  | idx < dst = ins s $ sendpackets (ss, pktlog) (idx+1) pkts
  | idx == dst = sendpackets (((addpacket s ds):ss),pktlog) idx ps
    where ins st (states,pkts) = (st:states,pkts)

network :: ([State], [Packet]) -> (Int, ([State], [Packet]))
network (states,pktlog) = (length packets, sendpackets (newstates, pktlog) 0 packets)
  where (newstates, rawpackets) = unzip $ map splitpacket states
        packets = sort $ filter (\x -> length x > 0) rawpackets

spin :: (Int, [State]) -> [Packet]
spin (pktless,states)
 | pktsend == 0 && pktless > 1000 && length natbuf > 1 =
   [(0,natdata)] ++ spin (0,(addpacket (head next) natdata) : tail next)
 | pktsend > 0 = newpkts ++ spin (0, next)
 | otherwise = spin (pktless + 1, next)
 where (pktsend, (next, newpkts)) = network (ticker states, [])
       natbuf = indata (next !! 50)
       natdata = drop (length natbuf - 2) natbuf

process :: String -> [String]
process bytes = map (show.last.snd) [head packets, dupnat packets]
    where packets = spin (0, map (\x -> newstate (parse bytes) [x]) [0..50])
          yval (_, (x:y:_)) = y
          dupnat ((0,a):(0,b):cs) = (0,a)
          dupnat (a:bs) = dupnat bs

main :: IO ()
main = interact (unlines . process)
