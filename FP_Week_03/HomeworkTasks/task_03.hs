-- Define a function that accepts a real number x, and a natural number (incl. 0) n
-- and calculates the n-th partial sum from a fancy sequence
-- IMPORTANT: The sequence in summation form looks like this:
-- Sum[i, 1, n] ((-2)^i * x^(i - 1) / (2 * i - 1)!!), where the '!!' is the
-- double-factorial function

main :: IO()
main = do
    -- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764

doubleFactIter :: Integer -> Integer
doubleFactIter n = helper 1 n -- 0!! = 1!! := 1
 where
    helper result 0 = result
    helper result 1 = result -- Two base cases
    helper result leftover = helper (result * leftover) (leftover - 2)

calcSeriesSum :: Double -> Integer -> Double -- The n-th partial sum is a natural number
calcSeriesSum _ 0 = -2 -- This is the base case, which, mathematically, could be put inside the Sum
calcSeriesSum x n = ((-2)^(n + 1) * x^n) / (fromIntegral $ doubleFactIter $ 2 * n + 1) + calcSeriesSum x (n - 1)