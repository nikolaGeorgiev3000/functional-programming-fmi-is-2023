-- Write a function that sums the unique numbers in the sublists of a list.

main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[1,-4],[1]] == 2
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45
    

sumUnique :: (Eq a, Num a) => [[a]] -> a
sumUnique xss = sum $ map sumUniqueList xss -- Sum for each of the sublists of the matrix of lists
 where
    sumUniqueList :: (Eq a, Num a) => [a] -> a
    sumUniqueList xs = sum $ filter ((==1) . (`count` xs)) xs

    count :: (Eq a) => a -> [a] -> Int
    count x = length . filter (==x)