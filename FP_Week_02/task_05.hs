-- Amicable numbers

main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumDivisors :: Int -> Int
sumDivisors 0 = 0
sumDivisors n = helper 2 1 -- 1 is a divisor for every number, so we start the sum from 1, and the divisors from 2.
 where
    helper d sum
     | d > abs n = sum -- "abs n" is added in order to add comparison for negative numbers as well.
     | mod n d == 0 = helper (d + 1) (sum + d)
     | otherwise = helper (d + 1) sum

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumDivisors x == sumDivisors y 