import Data.List

easyDigit :: String -> Bool
easyDigit s = elem (length s) [2, 4, 3, 7]

deduct :: [String] -> [String]
deduct leds = map (sort.head) [zero,one,two,three,four,five,six,seven,eight,nine]
    where getLen l ls = filter (\n -> l==length n) ls
          diff dig chars = filter (`notElem` (concat chars)) $ concat dig
          uniq l = concat $ getLen 1 $ group $ sort l
          one = getLen 2 leds
          seven = getLen 3 leds
          four = getLen 4 leds
          two_three_five = getLen 5 leds
          zero_six_nine = getLen 6 leds
          eight = getLen 7 leds
          beseg = uniq $ concat two_three_five
          eseg = head $ diff [beseg] four
          bseg = head $ diff [beseg] [[eseg]]
          two = filter (elem eseg) two_three_five
          five = filter (elem bseg) two_three_five
          three = uniq (two_three_five ++ two ++ five)
          nine = filter (notElem eseg) zero_six_nine
          cseg = head $ filter (/=eseg) $ diff two five
          six = filter (notElem cseg) zero_six_nine
          zero = uniq (zero_six_nine ++ six ++ nine)

decode :: ([String], [String]) -> Int
decode (segs, val) = read $ concat $ map pick val
    where nums = deduct segs
          charmap = zip nums (map show [0..9])
          pick v = head [ n | (sg,n) <- charmap, sg == sort v ]

parse :: String -> ([String], [String])
parse row = (take 10 w, drop 11 w)
    where w = words row

process :: [String] -> [String]
process rows = map (show.sum) [map (length.filter easyDigit.snd) parsed, map decode parsed]
    where parsed = map parse rows

main :: IO ()
main = interact (unlines . process . lines)
