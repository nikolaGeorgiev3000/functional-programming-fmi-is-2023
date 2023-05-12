-- Define a function that accepts a unary function and a list of numbers [y1, y2 .. yn] 
-- and returns a function that accepts x and calculates the expression: y1f(x) + y2f(x2) + .. + ynf(xn).
-- Imp. detail: Solve the task with one line of code and typeclasses! 

main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80 -- 0 * (2 + 2^1) + 1 * (2 + 2^2) + 2 * (2 + 2^3) + 3 * (2 + 2^4) 
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    -- print $ (sumExpr (/ 2.0) [1, 2]) 5 == 27.5 -- 1 * (5^1 / 2.0) + 2 * (5^2 / 2.0) = 2.5 + 25 

sumExpr :: (Num a, Num b) => (a -> b) -> [b] -> (a -> b)
sumExpr f ys = (\ x -> sum $ map (\ (yi, p) -> yi * f (x^p)) $ zip ys [1 ..])