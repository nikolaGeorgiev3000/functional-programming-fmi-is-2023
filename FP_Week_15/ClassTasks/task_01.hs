main :: IO()
main = do
    print $ listOfIndexes 3 [1, 2, 3, 4, 3, 5, 3, 2, 1] == [2, 4, 6]
    print $ listOfIndexes 4 [1, 2, 3, 2, 1, 2, 3, 2, 1] == []


listOfIndexes :: Int -> [Int] -> [Int]
listOfIndexes n xs = [ i | (i, number) <- indPairs, number == n] 
 where
    indPairs = zip [0 .. ] xs 