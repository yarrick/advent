import Control.Monad
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV

set vec (n,v) = MV.write vec n v
get vec n = MV.read vec (mod n $ MV.length vec)

build len input = do
    vec <- MV.replicate (succ len) (0 :: Int)
    mapM_ (set vec) $ zip [1..len] [2..]
    mapM_ (set vec) $ prep input (head input)
    return vec

target :: Int -> [Int] -> Int -> Int
target 1 pickup len= target len pickup len
target n pickup len
    | elem c pickup = target c pickup len
    | otherwise = c
    where c = pred n

-- takes extra unused arg to be used as a fold
move vec val _ = do
    fp <- get vec val
    np <- get vec fp
    lp <- get vec np
    let pickup = [fp, np, lp]
    let tgt = target val pickup (MV.length vec)
    -- disconnect pickup from list
    postpickup <- get vec lp
    set vec (val, postpickup)
    -- link pickup after target
    posttgt <- get vec tgt
    set vec (lp, posttgt)
    set vec (tgt, fp)
    return postpickup

nval :: UV.Vector Int -> Int -> Int
nval v n = v UV.! n

parse :: String -> [Int]
parse num = map (\c -> read [c]) num

prep :: [Int] -> Int -> [(Int,Int)]
prep [a] fst = [(a,fst)]
prep (a:b:cs) fst = (a,b) : prep (b:cs) fst

run input = do
    vec <- build (length input) input
    let val = head input
    vv <- foldM (move vec) val [1..100]
    iv <- UV.freeze vec
    putStrLn $ concatMap show $ tail $ take (length input) $ iterate (nval iv) 1

run2 input = do
    let vlen = 1000000
    vec <- build vlen input
    -- fix linking to use extra numbers
    set vec (last input, length input + 1)
    set vec (vlen, head input)
    let val = head input
    vv <- foldM (move vec) val [1..10000000]
    p1 <- get vec 1
    p2 <- get vec p1
    print $ p1 * p2

main = do
    content <- getContents -- read from stdin
    let input = parse $ head $ words content
    run input
    run2 input
