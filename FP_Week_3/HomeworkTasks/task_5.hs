-- Pentagonal function -> accepts a natural number n (excl. zero)
-- and returns the n-th pentagonal number

main :: IO()
main = do

    -- print $ pentagonalFormula 1 == 1
    -- print $ pentagonalFormula 2 == 5
    -- print $ pentagonalFormula 3 == 12
    -- print $ pentagonalFormula 4 == 22
    -- print $ pentagonalFormula 5 == 35
    -- print $ pentagonalFormula 6 == 51

    -- print $ altP 1 == 1
    -- print $ altP 2 == 5
    -- print $ altP 3 == 12
    -- print $ altP 4 == 22
    -- print $ altP 5 == 35
    -- print $ altP 6 == 51

    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

-- Fastest way
pentagonalFormula :: Int -> Int
pentagonalFormula n = div (n * (3 * n - 1)) 2

-- Incredibly slow, works, but for bigger n's, it dies out (FibonacciSeq Logic)
altP :: Int -> Int
altP 1 = 1
altP 2 = 5
altP n = 3 + 2 * altP (n - 1) - altP (n - 2)

-- Linearly iterative process
p :: Int -> Int
p n = helper 1 1
 where
    helper k pentNum
     | k == n = pentNum -- Base
     | otherwise = helper (k + 1) (pentNum + 3 * k + 1) 



