-- A function that removes every element, equal to x, in a list

main :: IO()
main = do
    print $ removeAllRec 5 [5] == []
    print $ removeAllRec 4 [4, 4] == []
    print $ removeAllRec 5 [1] == [1]
    print $ removeAllRec 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllRec 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

-- Using recursion
removeAllRec :: Int -> [Int] -> [Int]
removeAllRec _ [] = []
removeAllRec d (x:xs)
 | d == x = removeAllRec d xs -- We do not need 'x' in our result
 | otherwise = x : removeAllRec d xs -- We need 'x'

-- Using higher order functions
removeAllHOF :: Int -> [Int] -> [Int]
removeAllHOF d = filter (/= d) -- Get a filtered list ONLY with elements different that 'd'