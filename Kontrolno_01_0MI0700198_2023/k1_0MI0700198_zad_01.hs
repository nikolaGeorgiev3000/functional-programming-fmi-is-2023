-- "Silly solution" (does not work properly)
-- numDrink, numBottles, numExchange
-- We drink all numBottles, then we have emptyBottles = (numBottles + numExchange)
-- then we divide emptyBottles by numExchange. We add that number to numBottles 

-- Recursive thinking:
-- We drink (numExchange bottles from numBottles, then we drink one more. We keep doing that until we reach 0 bottles)

main :: IO()
main = do
    print $ numDrink 9 3   == 13 
    print $ numDrink 15 4  -- == 19 
    print $ numDrink 761 3 == 1141 
    
numDrink :: Int -> Int -> Int
numDrink numBottles numExchange
    | numBottles < numExchange = numBottles
    | otherwise = numBottles + numDrink (div numBottles numExchange + mod numBottles numExchange) numExchange

