drive :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
drive (h,v,_) ("up",len) = (h, v-len, 0)
drive (h,v,_) ("down",len) = (h, v+len, 0)
drive (h,v,_) ("forward",len) = (h+len, v, 0)

drive2 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
drive2 (h,v,aim) ("up", len) = (h,v,aim+len)
drive2 (h,v,aim) ("down", len) = (h,v,aim-len)
drive2 (h,v,aim) ("forward",len) = (h+len,v-aim*len,aim)

parse :: [String] -> (String, Int)
parse (dir:len:[]) = (dir, read len)

process :: [String] -> [String]
process rows = map res [foldl drive (0,0,0) steps, foldl drive2 (0,0,0) steps]
    where steps = map (parse.words) rows
          res (a,b,_) = show $ a * b

main :: IO ()
main = interact (unlines . process . lines)

