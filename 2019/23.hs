import Intcode
import Data.List
import Debug.Trace

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

sendpackets :: [State] -> Integer -> [[Integer]] -> [State]
sendpackets ss _ [] = ss
sendpackets (s:[]) 50 (p:ps) = trace ("Pkt to " ++ show (head p) ++ " data " ++ show (tail p))
  [(addpacket s p)]
sendpackets (s:ss) idx pkts@((dst:ds):ps)
  | idx < dst = s : sendpackets ss (idx+1) pkts
  | idx == dst = sendpackets ((addpacket s ds):ss) idx ps

network :: [State] -> (Int, [State])
network states = (length packets, sendpackets newstates 0 packets)
  where (newstates, rawpackets) = unzip $ map splitpacket states
        packets = sort $ filter (\x -> length x > 0) rawpackets

spin :: (Int, [State]) -> (Int, [State])
spin (pktless,states)
 | pktsend == 0 && pktless > 1000 && length natbuf > 1 =
  trace ("Sending NAT packet " ++ show natdata) $
   (0,(addpacket (head next) natdata) : tail next)
 | pktsend > 0 = (0, next)
 | otherwise = (pktless+1,next)
 where (pktsend, next) = network $ ticker states
       natbuf = indata (next !! 50)
       natdata = drop (length natbuf - 2) natbuf

run bytes = head $ drop 150000 $ iterate spin (0, map gen [0..50])
  where gen x = newstate (parse bytes) [x]
