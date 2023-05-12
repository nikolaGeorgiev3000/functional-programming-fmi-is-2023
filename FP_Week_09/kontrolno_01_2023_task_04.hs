main :: IO()
main = do
    print $ prefixToSuffix [] == []
    print $ prefixToSuffix [0] == [0]
    print $ prefixToSuffix [1, 3, 6, 10, 15] == [15, 14, 12, 9, 5]
    print $ prefixToSuffix [1, 3, 6, 10, 15] == [15, 14, 12, 9, 5]
    print $ prefixToSuffix [-1, -2, -3, -4, -5] == [-5, -4, -3, -2, -1]
    print $ prefixToSuffix [1, -4, 2, 90, 100, -1] == [-1, -2, 3, -3, -91, -101]
    print $ prefixToSuffix [1, 0, 1, 0, 1, 0, 1, 0] == [0, -1, 0, -1, 0, -1, 0, -1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 0, 0, 0, 1] == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    print $ prefixToSuffix [0, 0, 0, 0, 0, 0, 1, 1, 1, 1] == [1, 1, 1, 1, 1, 1, 1, 0, 0, 0]

getSuffixSums :: (Num a) => [a] -> [a]         -- From the original generating list, we acquire the list of suffix sums
getSuffixSums [] = []
getSuffixSums (x:xs) = sum (x:xs) : getSuffixSums xs

prefixToSuffix :: (Num a) => [a] -> [a]         -- First, we acquire the original gen. list from the list of prefix sums, then we apply getSuffixSums to it.
prefixToSuffix [] = []
prefixToSuffix (x:xs) = getSuffixSums $ x : helper (x:xs) 
 where
    helper [] = []
    helper [_] = []                           -- Used to handle lists with one element
    helper (x:y:xs) = (y - x) : helper (y:xs) -- For example, A(n - 1) = B(n - 1) - (A(0) + ... + A(n - 2)) = B(n - 1) - B(n - 2)