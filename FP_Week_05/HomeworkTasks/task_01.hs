 -- Define a function that returns the prime numbers in the range (x, y) that contain the digit 7.

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

isPrime :: Int -> Bool
isPrime p = p > 1 && null [d | d <- [2 .. p - 1], mod p d == 0]

containsSeven :: Int -> Bool
containsSeven n = elem '7' $ show n

-- LC in one line
getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [p | p <- [min (x + 1) (y + 1) .. max (x - 1) (y - 1)], isPrime p, containsSeven p]

-- HOF in one line
getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (\ n -> containsSeven n && isPrime n) [min (x + 1) (y + 1) .. max (x - 1) (y - 1)] -- An open interval in the condition