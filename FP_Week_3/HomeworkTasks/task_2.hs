-- Sort a number in descending order

main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence 0 _ = 0 
removeFirstOccurrence number digit = helper number 0 0 False 
 where
    helper :: Int -> Int -> Int -> Bool -> Int
    helper 0 result _ _ = result
    helper number result power removedDigit
     | mod number 10 == digit && not removedDigit = helper (div number 10) result power True           
     | otherwise = helper (div number 10) (result + 10^power * mod number 10) (power + 1) removedDigit 

findMax :: Int -> Int
findMax n = helper (div n 10) (mod n 10)
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result
     | mod leftover 10 > result = helper (div leftover 10) (mod leftover 10)
     | otherwise = helper (div leftover 10) result

countDigitOcc :: Int -> Int -> Int
countDigitOcc n d = helper n d 0
 where
    helper :: Int -> Int -> Int -> Int
    helper 0 _ counter = counter
    helper num digit counter
     | mod num 10 == digit = helper (div num 10) d (counter + 1)
     | otherwise = helper (div num 10) d counter

sortN :: Int -> Int
sortN n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = 10^(countDigitOcc n 0) * result
    helper num result = helper (removeFirstOccurrence num $ findMax num) (result * 10 + findMax num)