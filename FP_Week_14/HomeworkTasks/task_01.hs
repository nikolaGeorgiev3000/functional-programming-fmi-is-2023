{-
Define a function: getIndices:: [Int] -> (Int -> (Int, Int)).
The returned function accepts a whole number `n` as an argument,
and returns a pair (index1, index2) of numbers from the list ([Int]), 
which sum is equal to `n`. Return the indices of the first pair.
These indices always exist.
-}

main :: IO()
main = do
    print $ (getIndices [2, 7, 11, 15]) 9 == (0, 1) -- 2 + 7 = 9
    print $ (getIndices [3, 2, 4]) 6 == (1, 2)
    print $ (getIndices [3, 3]) 6 == (0, 1)

getIndices :: [Int] -> (Int -> (Int, Int))
getIndices xs = (\ n -> head [(i, j) | (i, x) <- indPairs, (j, y) <- indPairs, i /= j && x + y == n])
 where
    indPairs = zip [0 .. ] xs 