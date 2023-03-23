-- sumDivisibleNumbers start finish k -> returns the sum of all numbers from the 
-- closed interval [start, finish], whose digits sum up to a number that is div by k

main :: IO()
main = do
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDigits :: Int -> Int
sumDigits n = helper n 0
 where
    helper 0 result = result
    helper n result = helper (div n 10) (result + mod n 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish) k 
 where
    helper :: Int -> Int -> Int -> Int
    helper realStart realFinish divisor
     | realStart > realFinish = 0
     | mod (sumDigits realStart) divisor == 0 = realStart + helper (realStart + 1) realFinish divisor
     | otherwise = helper (realStart + 1) realFinish divisor