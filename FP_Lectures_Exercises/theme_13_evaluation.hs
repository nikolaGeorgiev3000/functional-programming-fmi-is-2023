
main :: IO()
main = do
    -- print $ take 5 pythagTriples
    -- print $ take 25 primes
    -- print $ isPrimeOrd primes 12 == False
    -- print $ isPrimeOrd primes 97 == True
    print $ nThPrimeNum 30


pythagTriples :: [(Int, Int, Int)]
pythagTriples = [(x, y, z) | z <- [2 .. ], y <- [2 .. z - 1], x <- [2 .. y - 1], x * x + y * y == z * z]


-- How to check whether a number is prime:
isPrimeOrd :: (Ord a) => [a] -> a -> Bool
isPrimeOrd (x:xs) num
 | x < num   = isPrimeOrd xs num    -- We keep checking until we reach the `num` or become (>) it in the inf. list.
 | x == num  = True
 | otherwise = False                -- We did not reach the `num` and we `skipped` it.             

nThPrimeNum :: Int -> Int
nThPrimeNum n = last $ take n primes

primes :: [Int]
primes = sieve [2 .. ]

sieve :: [Int] -> [Int]             -- Sieve of Eratosthenes
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]