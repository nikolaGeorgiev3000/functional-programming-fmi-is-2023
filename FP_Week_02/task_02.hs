-- Iterative function to calculate the sum
-- of the digits of a non-negative number

main :: IO()
main = do
   -- print $ sumDigitsIter (-13) -- error "n was negative"
   print $ sumDigitsIter 12345 == 15
   print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter n 
 | n < 0 = error "n was negative"
 | otherwise = helper n 0
 where
    helper 0 sum = sum
    helper n sum = helper (div n 10) (sum + mod n 10)