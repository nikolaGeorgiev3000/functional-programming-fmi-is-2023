-- sumNumbers start finish -> sum of all numberes in a closed interval
-- [start, finish], whose digits are ordered in a descending order

main :: IO()
main = do
    print $ sumNumbers 1 9 == 45
    print $ sumNumbers 199 203 == 200
    print $ sumNumbers 219 225 == 663
    print $ sumNumbers 225 219 == 663

isDescending :: Int -> Bool
isDescending n = n < 10 || mod n 10 <= mod (div n 10) 10 && isDescending (div n 10)
-- Either a single digit, or the last two digit are in desc. order AS WELL AS all the other "last two digits"

sumNumbers :: Int -> Int -> Int
sumNumbers x y = helper (min x y) (max x y)
 where
    helper :: Int -> Int -> Int
    helper realStart realFinish
     | realStart > realFinish = 0 -- We checked through the whole interval
     | isDescending realStart = realStart + helper (realStart + 1) realFinish -- We found a descending number, and we add it to the result
     | otherwise = helper (realStart + 1) realFinish