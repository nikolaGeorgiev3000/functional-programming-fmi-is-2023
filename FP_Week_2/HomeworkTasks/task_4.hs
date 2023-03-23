-- Number of occ of a given digit in a given pos. number

main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0
    print $ countOccurences 100 0 == 2
    print $ countOccurences 0 0 == 1

countOccurences :: Int -> Int -> Int
countOccurences 0 0 = 1 -- Corner-case which we need to take care of
countOccurences n digit = helper n digit 0
 where
    helper 0 digit counter = counter 
    helper n digit counter 
     | mod n 10 == digit = helper (div n 10) digit (counter + 1)
     | otherwise = helper (div n 10) digit counter