main :: IO()
main = do
    print $ (upperBound (*2) 100) 50 == 100
    print $ (upperBound (*2) 100.236) 500.002 == 1000.004
    print $ (upperBound (\x -> x) 1.001) 1.001 == 1.001

upperBound :: (Ord a) => (a -> a) -> a -> (a -> a)
upperBound f y = (\ x -> max y (f x)) -- Apply f to x, then take the bigger one of y and the result f(x)