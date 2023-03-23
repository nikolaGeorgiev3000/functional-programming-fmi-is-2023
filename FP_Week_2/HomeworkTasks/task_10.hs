-- Number of palindromes in an open interval between 2 numbers

main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

-- Changed the variable names of the arguments of the local functions
rev :: Int -> Int
rev x = helper x 0
 where
    helper 0 reversed = reversed
    helper p reversed = helper (div p 10) (reversed * 10 + mod p 10)

isPal :: Int -> Bool  
isPal n = n == rev n

countPalindromes :: Int -> Int -> Int
countPalindromes x y = helper (min (x + 1) (y + 1)) (max x y) 0 -- The min, max functions take care of the first number being bigger than the second 
 where
    helper p q counter
     | p >= q    = counter -- The inequality comes in handy when there is an empty interval of type (x, x)
     | isPal p   = helper (p + 1) q (counter + 1)
     | otherwise = helper (p + 1) q counter
