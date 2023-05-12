-- Returns True IFF the number is prime and every number that is produced by removing the last digit consequently, is prime as well

main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not

isPrime :: Int -> Bool
isPrime n = (n > 1) && helper 2 
 where
    helper d
     | d == n = True
     | mod n d == 0 = False
     | otherwise = helper (d + 1)

truncatablePrime :: Int -> Bool
truncatablePrime n = n < 10 && isPrime n || isPrime n && truncatablePrime (div n 10) -- The brackets seem unnecessary since '&&' has a higher prior. than '||'

{- Comments about the recursive function (PLEASE READ TO EVALUATE WHETHER I UNDERSTAND HOW IT WORKS):
The order of the check is from left to right. If we pass a single-digit prime number to the function, 
it evaluates the first conjunction, and returns 'True'. Let's say we pass '3797'. The first conjunction
evaluates to False in the first 3 checks (n >= 10). The first check goes to the right disjunction, 
evaluates to True && (callsRecursively). This repeats with the second, and third check. The fourth
check evaluates to True FROM THE LEFT DISJUNCTION (n < 10 && isPrime n == True), so now THERE ARE
TWO THINGS THAT COULD HAPPEN WHICH I DON'T UNDERSTAND -> 1) either the check stops here, and returns 
True (because the first part of the disjunction is evaluated to True); 2) or the recursive stack
starts evaluating one by one (FirstInLastOut). PLEASE, help to clarify these two options! :( -}