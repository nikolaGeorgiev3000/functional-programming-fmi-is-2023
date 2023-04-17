-- Define a function that returns the sum of the special numbers in the interval [x, y] (x <= y). 
-- A number is special if it contains 6 and can be expressed as 4k + 1, where k is a whole number.

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

containsSix :: Int -> Bool
containsSix n = elem '6' (show n)

canBeRepr :: Int -> Bool
canBeRepr n = mod (n - 1) 4 == 0

specialSum :: Int -> Int -> Int
specialSum x y = sum $ filter (\ n -> containsSix n && canBeRepr n) [x .. y]