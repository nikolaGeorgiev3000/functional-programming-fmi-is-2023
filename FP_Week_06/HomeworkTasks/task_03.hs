-- 1) sumRats - returns the sum of two rational numbers
-- 2) multiplyRats - returns the product of two rational numbers
-- 3) divideRats - returns the quotient of two rational numbers
-- 4) areEqual - returns whether two rational numbers are equal

main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True


type Rat a = (a, a)

normalize :: (Integral a) => Rat a -> Rat a
normalize (x, y) = (div x d, div y d)
 where
    d = gcd x y

sumRats :: (Num a, Integral a) => Rat a -> Rat a -> Rat a
sumRats (x1, y1) (x2, y2) = normalize (x1 * y2 + x2 * y1, y1 * y2)

multiplyRats :: (Num a, Integral a) => Rat a -> Rat a -> Rat a
multiplyRats (x1, y1) (x2, y2) = normalize (x1 * x2, y1 * y2)

divideRats :: (Num a, Integral a, Eq a) => Rat a -> Rat a -> Rat a
divideRats (x1, y1) (x2, y2)
 | y1 == 0 || y2 == 0 = error "Division by zero."
 | otherwise = normalize (x1 * y2, y1 * x2)

areEqual :: (Num a, Integral a, Eq a) => Rat a -> Rat a -> Bool
areEqual (x1, y1) (x2, y2) = x1 * y2 == x2 * y1