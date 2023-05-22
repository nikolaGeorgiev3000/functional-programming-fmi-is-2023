{-
Define a function that accepts an infinite list of numbers [x1, x2 .. ] 
and returns a function that for every x and y calculates the expression (x - x1)(x - x2) .. (x - x_y).
-}

main :: IO()
main = do
    print $ (myPoly [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998


myPoly :: (Num a) => [a] -> (a -> Int -> a)
myPoly xs = (\ x y -> multDiffs x (take y xs))

multDiffs :: (Num a) => a -> [a] -> a
multDiffs _ [] = 1
multDiffs x (z:zs) = (x - z) * multDiffs x zs