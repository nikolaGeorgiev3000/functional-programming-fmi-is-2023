-- Two inputs: x and y -> checks whether x is a sub-number of y

main :: IO()
main = do
    print $ subNum 123 5123783 == True -- x = 123, y = 5123783
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNum 1253 5123783 == False
    print $ subNum 12 0 == False

numDigits :: Int -> Int
numDigits 0 = 0
numDigits n = 1 + numDigits (div n 10)

subNum :: Int -> Int -> Bool
subNum x y = helper y (numDigits x)
 where
    helper :: Int -> Int -> Bool
    helper num lenX
     | x > num = False -- If x > num, it is imp. for x to be a sub-number of num
     | x == mod num (10^lenX) = True -- We check: (the last three digits of num == x)
     | otherwise = helper (div num 10) lenX -- Remove the last digit, and keep checking