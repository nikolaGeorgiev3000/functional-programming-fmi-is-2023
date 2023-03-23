-- A function that finds the sum of the elements in a list

main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

-- Lin. rec. without PM
mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | null xs = 0
 | otherwise = head xs + (mySumRecNonPM $ tail xs)

-- Lin. rec. with PM
mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

-- With func
mySumFunc :: [Int] -> Int
mySumFunc = sum