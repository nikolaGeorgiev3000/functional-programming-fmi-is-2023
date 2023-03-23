-- Reverse a non-negative number by linearly iterative process

main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Int -> Int
rev x 
 | x < 0 = error "Negative number"
 | otherwise = helper x 0
  where
    helper 0 reversed = reversed
    helper leftover reversed = helper (div leftover 10) (reversed * 10 + mod leftover 10) -- Removed the brackets around mod, since I learnt from my mistakes!