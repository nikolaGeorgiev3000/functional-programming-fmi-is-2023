-- Generate a list, made up of the numbers in [x, y]

main :: IO()
main = do
    print $ getClosedIntervalRec 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedIntervalRec 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]

    print $ getClosedIntervalOneLine 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedIntervalOneLine 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- Recursion
getClosedIntervalRec :: Int -> Int -> [Int]
getClosedIntervalRec x y = helper (min x y) (max x y)
 where
    helper :: Int -> Int -> [Int]
    helper realStart realFinish
     | realStart > realFinish = []
     | otherwise = realStart : helper (realStart + 1) realFinish -- fix the first el, call rec. for the rest



-- One line
getClosedIntervalOneLine :: Int -> Int -> [Int]
getClosedIntervalOneLine x y = [min x y .. max x y]