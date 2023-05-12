-- Write a function that, for a list xss whose elements are non-empty lists of numbers, 
-- returns a list of those elements of xss that represent an arithmetic progression.

main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]

isArithmetic :: (Num a, Eq a) => [a] -> Bool
isArithmetic xs = all (\ (x, y, z) -> y - x == z - y) $ zip3 xs (drop 1 xs) (drop 2 xs) -- Ex for zip3: zip3 [1, 2, 3] [2, 3] [3] == [(1, 2, 3)]

onlyArithmetic :: (Num a, Eq a) => [[a]] -> [[a]]
onlyArithmetic = filter isArithmetic 