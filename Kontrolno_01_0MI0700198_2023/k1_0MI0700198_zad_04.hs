-- Prefix sums:
-- B[0] = A[0]
-- B[1] = A[0] + A[1]
-- B[2] = A[0] + A[1] + A[2]
-- ...
-- B[n-1] = A[0] + A[1] + ... + A[n-1]

-- Suffix sums:
-- B[0] = A[0] + A[1] + A[2] + ... + A[n-1]
-- B[1] = A[1] + A[2] + ... + A[n-1]
-- ...
-- B[n-2] = A[n-2] + A[n-1]
-- B[n-1] = A[n-1]

main :: IO()
main = do
    print $ prefixToSuffix [1, 3, 6, 10, 15] == [15, 14, 12, 9, 5]
    print $ prefixToSuffix [0] == [0]
    print $ prefixToSuffix [-1, -2, -3, -4, -5] == [-5, -4, -3, -2, -1]
    print $ prefixToSuffix [1, -4, 2, 90, 100, -1] == [-1, -2, 3, -3, -91, -101]
    print $ prefixToSuffix [1, 0, 1, 0, 1, 0, 1, 0] == [0, -1, 0, -1, 0, -1, 0, -1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 0, 0, 0, 1] == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 1, 1, 1, 1] == [1, 1, 1, 1, 1, 1, 1, 0, 0, 0]

-- Let's analyse a connection between the element of the resulting list and the given list
-- prefixToSuffix [1, 3, 6, 10, 15] -> [15, (15 - 1), (15 - 1 - (3 - 1)), (15 - 1 - (3 - 1)) - (6 - 3), (15 - 1 - (3 - 1)) - (6 - 3) - (10 - 6)]
-- We need lin. iterative approach for that solution

prefixToSuffix :: (Num a) => [a] -> [a]
prefixToSuffix (x:xs) = helper (x:xs) ([last xs])
 where
    helper [] resultList = resultList
    helper (x1:x2:xs) resultList = helper (x2:xs) (resultList ++ [(last resultList) - (x2 - x1)])


-- reverse [1, 3, 6, 10, 15] -> [15, 10, 6, 3, 1]