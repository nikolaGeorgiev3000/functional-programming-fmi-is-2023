-- `numBottles` full bottles with water
-- each `numExchange` empty bottles can be replaced with a full bottle
-- we drink a full bottle, it becomes empty
-- given `numBottles`, `numExchange`, find max count of bottles that can be drunk

main :: IO()
main = do
    print $ numDrink 9 3 == 13
    print $ numDrink 15 4 == 19
    print $ numDrink 761 3 == 1141

numDrink :: Int -> Int -> Int
numDrink numBottles numExchange
 | numBottles < numExchange = numBottles                                         -- We cannot drink `numExch` numBottles and replace `numExchange` empty for one full, so we drink the numBottles
 | otherwise = numExchange + numDrink (numBottles - numExchange + 1) numExchange -- `numBottles - numExch + 1` because we drink `numExch`, and replace them for one full