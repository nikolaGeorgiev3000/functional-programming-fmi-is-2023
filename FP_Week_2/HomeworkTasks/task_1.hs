-- Recursive and Iterative approach for calculating the num
-- of digits of a non-negative number. Use guards.

main :: IO()
main = do
   --print $ countDigitsIter (-13) -- error "n was negative"
   print $ countDigitsIter 12345 == 5
   print $ countDigitsIter 123 == 3

   --print $ countDigitsRec (-13) -- error "n was negative"
   print $ countDigitsRec 12345 == 5
   print $ countDigitsRec 123 == 3

countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 0 = error "n was negative"
 | n < 10 = 1
 | otherwise = 1 + countDigitsRec (div n 10)

countDigitsIter :: Int -> Int
countDigitsIter 0 = 1 -- This is a corner-case, which is handled by countDigitsRec with n < 10, but with countDigitsIter it is not, so we take care of it
countDigitsIter n
 | n < 0 = error "n was negative"
 | otherwise = helper n 0 
  where
    helper 0 counter = counter
    helper n counter = helper (div n 10) (counter + 1)