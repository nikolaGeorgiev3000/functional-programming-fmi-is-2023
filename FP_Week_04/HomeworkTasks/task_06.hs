-- Define a function that returns the sum of the first n prime numbers that contain a digit d -> solve using HOF in one line
import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n $ filter (\ number -> isPrime number && elem (intToDigit d) (show number)) [2 .. ] 
-- Explanation: The lambda function works with two functions - isPrime and elem _ _ . The `intToDigit` function is used to convert d from Int to Char. Then we
-- check whether this character is amongst the characters of the string version of the number. This is done for all elements of the inf. list [2 .. ]. Since
-- Haskell has a lazy evaluation, it does not really calculate every prime, which has digit d in it. It only evaluates the first n of them, because of the `take`
-- function. Then we sum these 'n' numbers.