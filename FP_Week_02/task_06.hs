-- Interesting number - evenly divided by the sum of its digits

main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 

sumDigitsIter :: Int -> Int
sumDigitsIter n 
 | n < 0 = error "n was negative"
 | otherwise = helper n 0
 where
    helper 0 sum = sum
    helper n sum = helper (div n 10) (sum + mod n 10)

isInteresting :: Int -> Bool
isInteresting 0 = error "Division by 0 is not allowed."
isInteresting n = mod n (sumDigitsIter n) == 0 