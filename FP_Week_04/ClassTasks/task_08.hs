-- reverse, factorial, isPrime, sumDig, sumDivisors, IN ONE LINE
import Data.Char

main :: IO()
main = do 
    print $ rev 123 == 321

    print $ fact 5 == 120

    print $ isPrime 5 == True
    print $ isPrime 6 == False
    print $ isPrime 11 == True
    print $ isPrime 13 == True

    print $ sumDig 142500 == 12

    print $ sumDivs 161 == 192  

rev :: Int -> Int
rev = read . reverse . show 

fact :: Int -> Int
fact n = product [1 .. n]

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0] -- Explanation:
                                                               -- a number is prime IFF
                                                               -- it is bigger than 1
                                                               -- and the list of its
                                                               -- divisors consists only
                                                               -- of 1 and the number,
                                                               -- meaning, the list
                                                               -- [2 .. n - 1] must be empty

sumDig :: Int -> Int
sumDig = sum . map digitToInt . show -- digitToInt is from the Data.Char library

sumDivs :: Int -> Int
sumDivs n = sum $ filter (\ d -> mod n d == 0) [1 .. n] -- First, we filter a list, containing only
                                                       -- the divisors of n, then we sum the elements
                                                       -- of that list