import Data.Sequence as S

winner 1 = 1
winner 2 = 1
winner 3 = 3
winner 4 = 1
winner 5 = 3
winner 6 = 5
winner 7 = 7
winner 8 = 1
winner n
    | even n = pred $ 2 * winner (quot n 2)
    | subwin == 1 = n
    | otherwise = pred $ (pred subwin) * 2
    where subwin = winner (1 + quot (n-1) 2)

play2 :: S.Seq Int -> S.Seq Int
play2 q
    | S.length q == 1 = q
    | otherwise = play2 $ rotate $ pop q
    where pop s = S.deleteAt (S.length s `div` 2) s
          rotate t = S.drop 1 t >< S.take 1 t

winner2 n = S.index (play2 (S.fromList [1..n])) 0

process rows = map show [winner num, winner2 num]
    where num = read $ head rows

main :: IO ()
main = interact (unlines . process . lines)
