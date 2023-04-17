-- Odd indices functions application
-- Define a function that accepts a list of whole number unary functions [f1, f2 .. fn] and returns a function that 
-- for every x calculates the composition of the functions with odd indices: f1(f3(...(fnx)...)).

-- Implementation detail: Solve the task with one line of code using folding.

main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2 -- f1(f3(2)) === f1(1) === 2

getOddCompositionValue :: [(a -> a)] -> (a -> a)
getOddCompositionValue fs = (\ x -> foldr (\ (idx, f) acc -> if odd idx then f acc else acc) x $ zip [1 .. ] fs) -- Sensei's solution




-- getOddCompositionValue fs = (\ x -> foldr (\ f acc -> f acc) x (map snd $ filter (\ (i, _) -> odd i) $ zip [1 .. ] fs)) -- My solution





