-- Define a function that accepts a list of countries and returns the name of the country with the lowest 
-- average yearly temperature (the coldest country).

-- Implemenation detail: Solve the task using folding!
import Data.List (minimumBy)
import Data.Ord (comparing)

main :: IO()
main = do
    print $ coldestCountry [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ coldestCountry [(Country "Bulgaria" "Sofia" [(City "Varna" 0 (-16)), (City "Plovdiv" 120 34), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 (-15)), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]


-- Alternative solution (more usage of functional-level)
coldestCountry :: [Country] -> Name
coldestCountry = countryName . fst . minimumBy (comparing snd) . map (\ country -> (country, avgTemp $ cities country))
 where
    cities (Country _ _ cs) = cs
    avgTemp cs = (sum $ map (\ (City _ _ t) -> t) cs) / (fromIntegral $ length cs)      -- `cs` === list of cities
    countryName (Country n _ _) = n

{-
The `map` function is self-explanatory. 
`(comparing snd)` returns a function that compares two tuples based on their second element. 
The `minimumBy` function maps (I do not mean the `map` function :D) the list of tuples' values to different values, 
using the new `compare` function, and returns the pair with the minimum avgTemp. Then, we extract the first element of the pair,
and since this element is of the algebraic type `Country`, we apply the `countryName` function, which returns the Name (String) of the country.
-}

{-
-- Solution using folding
average xs = sum xs / (fromIntegral $ length xs)

coldestCountry :: [Country] -> Name
coldestCountry countries = foldl1 (\ countryName1 countryName2 -> if getAvgTemp countryName1 <= getAvgTemp countryName2 then countryName1 else countryName2) countryNames
 where
    getAvgTemp countryName = average [temp | (Country name _ cities) <- countries, (City _ _ temp) <- cities, name == countryName]
    countryNames = map (\ (Country name _ _) -> name) countries
-}