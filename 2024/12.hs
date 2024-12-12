import Data.List
import qualified Data.Map as M

type Pos = (Int, Int) -- row, col

process garden = map (show.sum.score) [length, length.(flines []).(sortBy fcomp)]
    where score fn = map (\(fs,c) -> (fn fs) * length c) $ areas garden

fcomp (adir,(ar,ac)) (bdir,(br,bc))
    | adir /= bdir = compare adir bdir
    | elem adir "NS" && ar /= br = compare ar br
    | elem adir "NS" = compare ac bc
    | ac /= bc = compare ac bc
    | otherwise = compare ar br

flines :: [(Char,Pos)] -> [(Char,Pos)] -> [[(Char,Pos)]]
flines prev [] = [prev]
flines [] (a:bs) = flines [a] bs
flines (p@(pdir,(pr,pc)):prev) (a@(adir,(ar,ac)):bs)
    | pdir /= adir = (p:prev) : flines [a] bs
    | elem pdir "NS" && ar /= pr = (p:prev) : flines [a] bs
    | elem pdir "NS" && ac /= succ pc = (p:prev) : flines [a] bs
    | elem pdir "NS" = flines (a:p:prev) bs
    | ac /= pc = (p:prev) : flines [a] bs
    | ar /= succ pr = (p:prev) : flines [a] bs
    | otherwise = flines (a:p:prev) bs

areas garden
    | M.null garden = []
    | otherwise = a : areas g
    where (pos,v) = (head $ M.toList garden)
          (a, g) = grow v [] [] [pos] (M.delete pos garden)

grow :: Char -> [(Char,Pos)] -> [Pos] -> [Pos] -> M.Map Pos Char -> (([(Char,Pos)], [Pos]), M.Map Pos Char)
grow ch f area [] garden = ((f, area), garden)
grow ch f prev ((r,c):todo) garden = grow ch (f++fences) ((r,c):prev) (todo++newsides) $ foldr M.delete garden ((r,c):newsides)
    where cands = [('N', (r-1,c)), ('W',(r,c-1)), ('E',(r,c+1)), ('S',(r+1,c))]
          known = filter (\p -> elem p (prev ++ todo)) $ map snd cands
          newsides = filter (\p -> M.member p garden && garden M.! p == ch) $ map snd cands
          fences = filter (\(_,p) -> notElem p known && notElem p newsides) cands


parse :: [String] -> M.Map Pos Char
parse ss = M.fromList $ concatMap inject $ zip [0..] $ map (zip [0..]) ss
    where inject (r,cs) = map (\(c,v) -> ((r,c),v)) cs

main :: IO ()
main = interact (unlines . process . parse . lines)
