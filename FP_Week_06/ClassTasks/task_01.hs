--  Define a function that accepts a unary function and a list of numbers [y1, y2 .. yn] 
--  and returns a function that for every x calculates the expression: f(y1x) + 2 f(y2x) + .. + n f(ynx).

main :: IO()
main = do
    print $ (myPolynomial (\x -> x - 2) []) 5 == 0
    print $ (myPolynomial (\x -> x + 10) [3.62, 6.12, 9.45, 8.02, 5, 2]) (-5)== -356.45
    print $ (myPolynomial (\x -> x - 2) [1, 4, 7, 8, 5, 2]) 5 == 453

myPolynomial :: (Num a, Enum a) => (a -> a) -> [a] -> (a -> a)
myPolynomial f ys = (\ x -> sum [i * f (x * yi) | (yi, i) <- zip ys [1 ..]])
-- Explanation: `x` is given by the user-input. We use Num & Enum in order to use the operators `*` and `+`. `i` is 1, 2, ... according to the number of elements
-- in `ys` list. `yi` are the sequential elements of `ys`. Every additive has a form `i * f (x * yi)`.