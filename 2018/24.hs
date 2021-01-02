import Data.Char
import Data.List

-- units, hit points, attack damage, attack type, initiative, weaknesses, immunity
type Squad = (Int, Int, (Int, String), Int, [String], [String])

decode :: [[String]] -> Squad
decode (header:rest)
    | null rest = build (hparse header) (aparse $ drop 7 header) [] []
    | otherwise = build (hparse header) (aparse $ last rest) (modifiers "weak" rest) (modifiers "immune" rest)
    where hparse (uval:_:_:_:hval:uhs) = (read uval, read hval)
          aparse ("with":"an":"attack":_:_:aval:akind:_:"at":"initiative":ival:is) = (read aval, akind, read ival)
          modifiers md chunks = concatMap (drop 2) $ filter (\l -> length l > 0 && head l == md) chunks
          build (units, hits) (damage,dkind,itive) weak immune = (units, hits, (damage, dkind), itive, weak, immune)

parse :: [String] -> [(Int, Squad)]
parse rows = (zip [1..] $ handle a) ++ (zip (map negate [1..]) $ handle b)
    where (a,b) = break (=="") rows
          slice = map (map (filter isAlphaNum).words) . groupBy (\a b -> isSep a == isSep b)
          handle t = map (decode.slice) $ tail $ filter (/="") t
          isSep cc = elem cc "(;)"

epower :: Squad -> Int
epower (units,_,(aval,_),_,_,_) = units * aval

initiative :: Squad -> Int
initiative (_,_,_,itiv,_,_) = itiv

units :: Squad -> Int
units (n,_,_,_,_,_) = n

damage :: Squad -> Squad -> Int
damage sq@(units,_,(aval, akind),_,_,_) (_,_,_,_,weak,immune)
    | elem akind immune = 0
    | elem akind weak = 2 * epower sq
    | otherwise = epower sq

pick :: (Int, Squad) -> [(Int, Squad)] -> [(Int,Int,Int,Squad)]
pick (tag,sq) tgts = take 1 $ sortBy pcmp $ filter (\(d,_,_,_) -> d > 0) $ dmgs tgts
    where dmgs ts = map (\s -> (damage sq (snd s), tag, fst s, snd s)) $ filter (\(n,_) -> signum n /= signum tag) ts
          pcmp (admg,_,_,asq) (bdmg,_,_,bsq)
            | admg /= bdmg = compare bdmg admg
            | ae /= be = compare be ae
            | otherwise = compare (initiative bsq) (initiative asq)
            where ae = epower asq
                  be = epower bsq

-- returns (attacker, defender) squad numbers
targeting :: [(Int,Squad)] -> [(Int,Squad)] -> [(Int,Int)]
targeting [] _ = []
targeting ss tgts
    | null picked = targeting sqs tgts
    | otherwise = (atag, etag) : targeting sqs (filter (\(tt,_) -> tt /= etag) tgts)
    where tcmp (_,a) (_,b)
            | epower a /= epower b = compare (epower b) (epower a)
            | otherwise = compare (initiative b) (initiative a)
          (sq:sqs) = sortBy tcmp ss
          picked = pick sq tgts
          (_,atag,etag,_) = head picked

attack :: ([((Int,Squad),Int)], [((Int,Squad), Int)]) -> [(Int,Squad)]
attack (f,[]) = map fst f
attack (fought, hitter@((tag,sq),target):rest)
    | target == 0 = attack (hitter:fought, rest)
    | not tfought = attack (hitter:fought, sortBy hcmp ((hit $ tgt rest) : other rest))
    | otherwise = attack (hitter:(hit $ tgt fought):(other fought), rest)
    where tfought = elem target $ map (fst.fst) fought
          tgt list = head $ filter (\((t,_),_) -> t == target) list
          other list = filter (\((t,_),_) -> t /= target) list
          hit ((n,tsq@(units,hp,att,itive,w,i)),attr)
            | kills >= units = ((n,(0,hp,att,itive,w,i)),attr)
            | otherwise = ((n,(units - kills,hp,att,itive,w,i)),attr)
            where kills = damage sq tsq `quot` hp
          hcmp (a,_) (b,_) = icmp a b

icmp :: (Int,Squad) -> (Int,Squad) -> Ordering
icmp (_,a) (_,b) = compare (initiative b) (initiative a)

fight :: [(Int,Squad)] -> [(Int,Squad)]
fight squads = attack ([], map target $ sortBy icmp squads)
    where attacks = targeting squads squads
          target (a,b)
            | elem a (map fst attacks) = ((a,b),snd $ head $ filter (\(att,def) -> att == a) attacks)
            | otherwise = ((a,b),0)

battle :: [(Int,Squad)] -> [(Int,Squad)]
battle sqs
    | teams == 1 = alive
    | totunits sqs == totunits result = [] -- stop if fight reaches a draw
    | otherwise = battle result
    where alive = filter (\(_,(n,_,_,_,_,_)) -> n > 0) sqs
          totunits ss = sum $ map (units.snd) ss
          result = fight alive
          teams = length $ nub $ map (signum.fst) alive

boosted :: [(Int,Squad)] -> Int -> (Bool,Int,Int)
boosted sqs bval = (not $ null rimm, sum $ map (units.snd) rimm, sum $ map (units.snd) rinf)
    where teams tt = partition (\(t,_) -> signum t == 1) tt
          (imm, infec) = teams sqs
          boost (tag,(n,hp,(aval,akind),itive,wk,im)) = (tag,(n,hp,(aval+bval,akind),itive,wk,im))
          (rimm,rinf) = teams $ battle $ concat [map boost imm, infec]

lowboost :: [(Int,Squad)] -> Int -> Int -> Int -> (Int,Int)
lowboost sqs loval hival range
    | res && range == 1 = (checked, survivors)
    | range == 1 = (hival, hsurv)
    | res = lowboost sqs loval (loval+range) (range `quot` 2)
    | otherwise = lowboost sqs (checked+1) hival (range `quot` 2)
    where checked = loval + range
          (res,survivors,enemies) = boosted sqs checked
          (_,hsurv,_) = boosted sqs hival

process :: [(Int, Squad)] -> [String]
process squads = map show [sum $ map (units.snd) winners, snd $ lowboost squads 0 32768 16384]
    where winners = battle squads

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . parse . lines)
