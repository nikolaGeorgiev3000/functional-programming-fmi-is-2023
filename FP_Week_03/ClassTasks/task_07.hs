-- sine function -> takes n (the number of partial sum) and a point x (in radians)
-- Reference with the Maclaurin Series

main :: IO()
main = do
    print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
    print $ mySin 100 0.5 == 0.479425538604203

-- Need the factorial function for the denominator
factIter :: Integer -> Integer -- Integer is probably used for complete accuracy.
factIter n = helper 1 n -- 0! = 1
 where
    helper result 0 = result
    helper result leftover = helper (result * leftover) (leftover - 1)

mySin :: Integer -> Double -> Double
mySin 0 x = x -- The 0-th partial sum is just the point the user inputs
mySin n x = ((-1)^n * x^(2 * n + 1)) / (fromIntegral $ factIter $ 2 * n + 1) + mySin (n - 1) x
-- Explanation: 
-- Start with calculating the n-th term, the continue going down until you reach the 0-th
-- partial sum, which is def. to be x. In the numerator it's easy, the denominator needs the fromIntegral funct.
-- in order to be able to get a correct/accurate Double result. "+ mySin (n - 1) x" calls rec. every other term.
-- Note: In the denominator, we use two '$' operators -> from right to left the first one is used to tell
-- the factIter function to wait the result from (2 * n + 1). The second one is used to tell the function 
-- fromIntegral to wait for the result of the factIter function.