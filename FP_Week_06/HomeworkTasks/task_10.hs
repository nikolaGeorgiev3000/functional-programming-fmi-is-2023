-- Define a function that accepts a number and returns the tuple (x, y) 
-- where x is the sum of the digits on even indices of the number and y 
-- is the sum of the ones on odd indices.
-- Implementation detail: Solve the task with one line of code using folding.
import Data.Char (digitToInt)

main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)

checkNumber :: Int -> (Int, Int)
checkNumber n = foldl (\ (x, y) (i, d) -> if even i then (x + d, y) else (x, y + d)) (0, 0) $ zip [0 .. ] $ map digitToInt $ show n
-- Quick/Informal explanation: Convert the number to a list of characters, convert each character to its corresponding Int value, 
-- zip into a list of pairs [(0, firstDigit), (1, secondDigit), ...], use left folding with a starting acc === (0, 0) (which is being 'connected' to (x, y)),
-- and (i, d), which is cycling through the list of pairs.