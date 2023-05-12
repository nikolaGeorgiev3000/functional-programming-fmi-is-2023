-- Define a function that checks whether the digits of a non-negative number are ordered in non-decreasing order -> one line without div or mod

main :: IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False    

isAscending :: Int -> Bool
isAscending = all (uncurry (<=)) . (zip <*> tail) . show -- Example: 12334 -> "12334" -> [(1, 2), (2, 3), (3, 3), (3, 4)] -> 1 <= 2 && 2 <= 3 && 3 <= 3 && 3 <= 4 -> True
                                                         -- The `show` function converts an Int into a String. Then, the expression (zip <*> tail) first applies tail to 
                                                         -- the string, which returns a new string with the first character removed. Then, it applies zip to the original 
                                                         -- string (the result from `show`) and the resulting tail string to get a list of adjacent digit pairs. The `(uncurry (<=))`
                                                         -- function takes a pair of numbers, and checks them with `<=` operator. It does that for all of the pairs, because of
                                                         -- the `all` function, which is a BIG CONJUNCTION.