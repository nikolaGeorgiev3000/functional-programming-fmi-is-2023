-- Bank accounts and people

main :: IO()
main = do
    print $ getAverageBalance (accounts1, people1) (\ (_, _, city) -> city == "Burgas") == 24.95
    print $ getAverageBalance (accounts1, people1) (\ (_, (n:_), _) -> n == 'P') == 18.85
    print $ getAverageBalance (accounts1, people1) (\ (n, _, _) -> n == 2) == 26.8
    
    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62
    print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25


type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1, "Ivan", "Sofia"),(2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"),(4, "Petya", "Burgas")]

accounts1 :: [Account]
accounts1 = [(1, 1, 12.5),(2, 1, 123.2),(3, 2, 13.0),(4, 2, 50.2),(5, 2, 17.2),(6, 3, 18.3),(7, 4, 19.4)]

roundTwoDig :: Double -> Double
roundTwoDig x = (fromIntegral $ round $ x * 100) / 100.0

getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (accounts, people) p = roundTwoDig $ sum xs / (fromIntegral $ length xs)
 where
    xs = [money | (_, idAcc, money) <- accounts, per@(idPer, _, _) <- people, p per, idAcc == idPer]

averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities db xs = getAverageBalance db (\ (_, _, city) -> elem city xs)


-- A little worse looking solution:

{-
findPersonById :: Int -> [Person] -> Person
findPersonById personId = head . filter (\(id, _, _) -> id == personId)

-- Helper function to find the city of a person by their ID in the list of people
findCityByPersonId :: Int -> [Person] -> String
findCityByPersonId personId people = (\ (_, _, city) -> city) $ findPersonById personId people

-- The following function is used to match the test cases
roundToTwoDecimals :: Double -> Double
roundToTwoDecimals num = (fromIntegral $ round $ num * 100) / 100


getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (accounts, people) predicate
 | totalAccounts == 0 = 0
 | otherwise = roundToTwoDecimals $ totalBalance / fromIntegral totalAccounts
    where
     filteredAccounts = filter (\ (accId, personId, balance) -> predicate (findPersonById personId people)) accounts
     totalBalance = sum $ map (\ (_, _, balance) -> balance) filteredAccounts
     totalAccounts = length filteredAccounts

averageBalanceOfCities :: ([Account], [Person]) -> [String] -> Double
averageBalanceOfCities (accounts, people) cities
 | totalAccounts == 0 = 0
 | otherwise = roundToTwoDecimals $ totalBalance / fromIntegral totalAccounts
    where
     filteredAccounts = filter (\ (accId, personId, balance) -> elem (findCityByPersonId personId people) cities) accounts
     totalBalance = sum $ map (\ (_, _, balance) -> balance) filteredAccounts
     totalAccounts = length filteredAccounts

-- NOTE: There is repetition of the locally defined functions, but even if we define them globally, we would still need to pass parameters to them,
-- and it would still get messy.
-}