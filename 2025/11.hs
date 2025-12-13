import qualified Data.Map as M

paths :: M.Map String [String] -> M.Map String (M.Map String Int) -> [(String,Int)] -> M.Map String (M.Map String Int)
paths _ res [] = res
paths tree res ((node,val):ns) = paths tree nextres (ns ++ map nextnode tgts)
    where tgts = tree M.! node
          putval m t = M.insert t (M.insert node val submap) m
            where submap = M.findWithDefault (M.empty) t m
          nextres = foldl putval res tgts
          nextnode t = (t, sum $ M.elems $ nextres M.! t)

process pos = [show $ sum $ M.elems $ res M.! "out"]
    where tree = M.insert "out" [] $ M.fromList pos
          res = paths tree (M.empty) [("you",1)]

parse s = (filter (':'/=) dev, outs)
    where (dev:outs) = words s

main :: IO ()
main = interact (unlines . process . map parse . lines)
