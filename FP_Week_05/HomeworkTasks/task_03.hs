-- Check whether a seqence of numbers forms an arithmetic progression.
-- Hint: Use the `!!` operator

main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False

isArithmetic :: [Int] -> Bool
isArithmetic [] = True 
isArithmetic [_] = True
isArithmetic xs = all (== d) (zipWith (-) (tail xs) xs) -- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]. We check every consequential diff: (tail xs) - xs, but one by one with zipWith
 where d = xs!!1 - xs!!0 -- d is def. to be the difference between and second and the first element
