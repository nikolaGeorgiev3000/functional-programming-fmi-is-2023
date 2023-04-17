main :: IO()
main = do
    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)

    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053

type Vector a = (a, a, a)

-- NOTE: We are using `(Floating a) => ...` in order to work with the best `type signature` with vectors.
dotProduct :: (Floating a) => Vector a -> Vector a -> a
dotProduct (x, y, z) (x', y', z') = x * x' + y * y' + z * z'

crossProduct :: (Floating a) => Vector a -> Vector a -> Vector a
crossProduct (x, y, z) (x', y', z') = (y * z' - z * y', -(x * z' - z * x'), x * y' - y * x')

magnitude :: (Floating a) => Vector a -> a -- Floating is necessary because of the sqrt
magnitude (x, y, z) = sqrt $ x * x + y * y + z * z