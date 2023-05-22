{-
Define a function that accepts a list of countries and returns the name of the country 
with the highest capital,  i.e. the capital on the highest elevation.
Implementation detail: Solve the task using folding.
-}
-- import Data.List (maximumBy)
-- import Data.Ord (comparing)

main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]),
                            (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), 
                            (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] 
                            == "Bulgaria"

highestCapital :: [Country] -> Name
-- highestCapital countries = fst $ maximumBy (comparing snd) countriesAndCapElevs
highestCapital countries = fst $ foldr1 (\ p1@(_, elev1) p2@(_, elev2) -> if elev1 > elev2 then p1 else p2) countriesAndCapElevs
 where
    countriesAndCapElevs                  = [(getCountryName country, getCapitalElev country) | country <- countries]
    getCountryName (Country name _ _)     = name
    getCapitalElev (Country _ cap cities) = getCityElevation cap cities
    getCityElevation city (City name elev _ : rest)
     | city == name = elev
     | otherwise   = getCityElevation city rest


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]