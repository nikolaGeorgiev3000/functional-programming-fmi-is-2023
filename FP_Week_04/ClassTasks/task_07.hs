-- Increments all values in a list by a given number

main :: IO()
main = do
    print $ incrementByLC 5 [5] == [10]
    print $ incrementByLC 4 [4, 4] == [8, 8]
    print $ incrementByLC 5 [1] == [6]
    print $ incrementByLC 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByLC 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]

    print $ incrementByHOF 5 [5] == [10]
    print $ incrementByHOF 4 [4, 4] == [8, 8]
    print $ incrementByHOF 5 [1] == [6]
    print $ incrementByHOF 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByHOF 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]

-- List comprehension
incrementByLC :: Int -> [Int] -> [Int]
incrementByLC d xs = [x + d | x <- xs] -- The list of comprised of these elements
                                       -- 'x + d', in which 'x' can be found in the 
                                       -- list 'xs' (or in which 'x' is an element)
                                       -- of the list 'xs'
-- HOF
incrementByHOF :: Int -> [Int] -> [Int]
incrementByHOF d = map (+d) -- Cycles through all elements of the given list-arg,
                            -- and increments all of them by d. Simple logic.