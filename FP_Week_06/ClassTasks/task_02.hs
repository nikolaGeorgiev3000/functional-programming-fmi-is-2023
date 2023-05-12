-- Define a function that accepts two unary functions f and g and a list of values and checks whether f dominates g. We say that one function 
-- dominates another if for every value the absolute value after applying f is greater than or equal to the absolute value after applying g.
-- Implementation detail: Solve the task with one line of code using folding.
-- Note: we will use `all` as well for alternative solution

main :: IO()
main = do 
    print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True
    
    print $ dominatesAlt (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominatesAlt (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True

dominates :: (Ord a, Num a) => (a -> a) -> (a -> a) -> [a] -> Bool
dominates f g = foldl (\ acc x -> acc && abs (f x) >= abs (g x)) True

-- Easier alternative
dominatesAlt :: (Ord a, Num a) => (a -> a) -> (a -> a) -> [a] -> Bool
dominatesAlt f g = all (\ x -> abs (f x) >= abs (g x))