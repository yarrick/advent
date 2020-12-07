
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
