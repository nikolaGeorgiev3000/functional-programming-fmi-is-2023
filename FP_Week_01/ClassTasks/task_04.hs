
fibRec :: Int -> Int
fibRec n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fibRec(n - 2) + fibRec(n - 1)