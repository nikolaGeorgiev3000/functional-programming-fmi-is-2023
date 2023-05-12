-- Remove the digit d from the number n

main :: IO ()
main = do 
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134

-- My solution: result = 0, and then we only add digits from our number, which are not equal to d. The rev function helps us have a normal-order result
rev :: Int -> Int
rev x = helper x 0
 where
    helper 0 reversed = reversed
    helper x reversed = helper (div x 10) (reversed * 10 + mod x 10)

removeD :: Int -> Int -> Int
removeD d n = helper (rev n) 0
 where
    helper 0 result = result
    helper n result 
     | mod n 10 == d = helper (div n 10) result
     | otherwise = helper (div n 10) (result * 10 + mod n 10)

-- "Advanced" online solution -> uses casting to char, and works with strings/symbols, then goes back to an Int. Let's hope we reach that point someday... :)
removeDAdv :: Int -> Int -> Int
removeDAdv d n = read $ filter (/= digitChar) $ show n
 where digitChar = head $ show d