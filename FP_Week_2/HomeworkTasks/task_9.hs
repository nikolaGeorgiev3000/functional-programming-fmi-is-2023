-- Form a number from every other digit, starting from the penultimate digit, from right to left (assume n >= 10)

main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14

everyOther :: Int -> Int
everyOther n = helper n 0 0 -- Start the counter from zero
 where
    helper 0 counter result = result
    helper n counter result 
     | even counter = helper (div n 10) (counter + 1) result                -- Skip this digit
     | otherwise = helper (div n 10) (counter + 1) (result * 10 + mod n 10) -- Free a slot and use this digit