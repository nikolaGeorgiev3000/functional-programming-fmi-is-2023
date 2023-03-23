-- A func that returns the prime numbers in the range (x, y), which contain the dig 7
import Data.Char

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]


-- List Compr in one line
isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

containsSeven :: [Int] -> Bool
containsSeven (x:xs) = x == 7 || (not $ null xs) && containsSeven xs

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [p | p <- [min x y + 1 .. max x y - 1], isPrime p, containsSeven $ map digitToInt $ show p]

-- HOF in one line
getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (containsSeven . map (\c -> read [c]) . show) $ filter isPrime [min x y + 1 .. max x y - 1]

-- Explanation for the last function:
-- First, we filter out all the prime numbers in the given range.
-- This returns a list of prime numbers between x and y (exclusive)). Next, we use the show function to convert each prime number to a string. 
-- We then apply the map function to each string to convert each character in the string to an integer. This function takes a character c, converts 
-- it to a string using the [] operator, and then converts that string to an integer using the read function. Finally, we apply the containsSeven function 
-- to the resulting list of integers to check if it contains the digit 7. If it does, the prime number is included in the final list. If it doesn't, 
-- the prime number is filtered out.