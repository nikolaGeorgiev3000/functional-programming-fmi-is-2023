-- A function that removes the first el in a list that is equal to x (from left to right)

main :: IO()
main = do
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeFirst :: Int -> [Int] -> [Int]
removeFirst _ [] = [] -- You can't remove an element from an empty list (base case)
removeFirst d (x:xs)
 | d == x = xs -- If we enter this condition, we do not need the element 'x' for our result
 | otherwise = x : removeFirst d xs 