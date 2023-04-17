-- Define a function that takes a single argument function and returns it applied n times.

main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1 


-- Solution_1
-- applyN :: (a -> a) -> Int -> (a -> a)
-- applyN f n = (\x -> iterate f x !! n) 

-- Solution_2
applyN :: (a -> a) -> Int -> (a -> a)
applyN _ 0 = id
applyN f n = (\ x -> (applyN f (n - 1)) (f x))