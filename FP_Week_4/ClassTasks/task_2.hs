-- Return the number of elements of a list

main :: IO()
main = do
    print $ myLengthRecNonPM [] == 0
    print $ myLengthRecNonPM [1, 2, 3] == 3

    print $ myLengthRecPM [] == 0
    print $ myLengthRecPM [1, 2, 3] == 3

    print $ myLengthFunc [] == 0
    print $ myLengthFunc [1, 2, 3] == 3


-- With functions
myLengthFunc :: [Int] -> Int
myLengthFunc = length -- Easy-peasy

-- LCP without PM
myLengthRecNonPM :: [Int] -> Int
myLengthRecNonPM xs
 | null xs = 0 -- The base case
 | otherwise = 1 + (myLengthRecNonPM $ tail xs) -- 1 + recCall (all els but the first)

-- LCP with PM
myLengthRecPM :: [Int] -> Int
myLengthRecPM [] = 0
myLengthRecPM (_:xs) = 1 + myLengthRecPM xs
