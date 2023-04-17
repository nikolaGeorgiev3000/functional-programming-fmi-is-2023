-- Define a function that returns the sum of the smallest and greatest palindrome divisors of a natural number greater than 1.
main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364
    -- print $ getPalindromesHOF 132465 == 8
    -- print $ getPalindromesHOF 654546 == 8
    -- print $ getPalindromesHOF 100001 == 100012
    -- print $ getPalindromesHOF 21612 == 21614
    -- print $ getPalindromesHOF 26362 == 26364

isPal :: Int -> Bool
isPal n = n == (read $ reverse $ show n)

sumFstLst :: [Int] -> Int
sumFstLst xs = head xs + last xs

-- NOTE: The calculation is incredibly slow, since it has to calculate up until the greatest div, which is palindrome. Have no clue how to optimize it.

-- Using LC
getPalindromes :: Int -> Int
getPalindromes n 
 | n < 1 = error "Enter a number greater than 1."
 | otherwise = sumFstLst [palDiv | palDiv <- [2 .. n], isPal palDiv, mod n palDiv == 0]

-- Using HOF
-- getPalindromesHOF :: Int -> Int
-- getPalindromesHOF n = sumFstLst $ filter (\ d -> isPal d && mod n d == 0) [2 .. n]