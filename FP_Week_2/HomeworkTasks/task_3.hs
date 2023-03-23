-- Define a function that returns the sum of 
-- all prime divisors of a given number

main :: IO()
main = do 
   print $ sumPrimeDivs 0 == 0
   print $ sumPrimeDivs 6 == 5 -- 2 + 3
   print $ sumPrimeDivs 18 == 5 -- 2 + 3
   print $ sumPrimeDivs 19 == 19
   print $ sumPrimeDivs 45136 == 53

isPrime :: Int -> Bool
isPrime n = helper 2 -- Divisors start from 2 in sumPrimeDivs, so there's no need for (n > 1) in a conjunction
 where
   helper d
    | d == n = True
    | mod n d == 0 = False
    | otherwise = helper (d + 1)

sumPrimeDivs :: Int -> Int
--sumPrimeDivs 0 = 0 
sumPrimeDivs n = helper n 2 0
 where
   helper n d sum
    | d > n = sum
    | mod n d == 0 && isPrime d = helper n (d + 1) (sum + d) 
    | otherwise = helper n (d + 1) sum

