-- Remove the first occ. of a digit in a number, going from right to left

main :: IO()
main = do
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 10100 1 == 1000
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565 
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

-- Lin. it. approach
removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence 0 _ = 0 
removeFirstOccurrence number digit = helper number 0 0 False 
 where
    helper :: Int -> Int -> Int -> Bool -> Int
    helper 0 result _ _ = result
    helper number result power removedDigit
     | mod number 10 == digit && not removedDigit = helper (div number 10) result power True           -- Not going in this case WHEN ONCE ENTERED!
     | otherwise = helper (div number 10) (result + 10^power * mod number 10) (power + 1) removedDigit -- The result here is the "old" digit/digits, we free up a slot, and put the "new digit" (mod number 10) there. 
                                                                                                       -- We increase the power by 1, and keep the removedDigit value 

-- Alternative solution, using lists, down below:
{-
-- Int to a List
digs :: Int -> [Int]
digs 0 = []
digs x = digs (div x 10) ++ [mod x 10]

-- List to an Int
fromDigits :: [Int] -> Int
fromDigits = foldl helper 0
 where 
    helper num d = 10 * num + d

-- Concat a number and a list, and return a number
concatNumList :: Int -> [Int] -> Int
concatNumList x y = fromDigits $ (digs x ++ y) 

-- Append an element to the back of the list
appendElement :: Int -> [Int] -> [Int]
appendElement num xs = xs ++ [num]

-- Try to write the leftover as a list
removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence n d = helper n d [] -- Start with the empty list
 where
    helper :: Int -> Int -> [Int] -> Int
    helper num digit leftover 
     | mod num 10 == digit = concatNumList (div num 10) (reverse leftover)
     | otherwise = helper (div num 10) digit (appendElement (mod num 10) leftover) -- The leftover is being filled
-}
