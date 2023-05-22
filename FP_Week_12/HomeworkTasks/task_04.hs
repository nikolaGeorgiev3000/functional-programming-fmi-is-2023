
main :: IO()
main = do
    print $ closestToAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] -- 6 or 11 or 21
    -- print $ getAvgTemp [] -- === NaN

data Measuring = Temp Int Float -- `Int` for day of the month, `Float` for the temp. of that day.

closestToAverage :: [Measuring] -> Int
closestToAverage ms = getDay $ getClosestMeasuring ms (getAvgTemp ms)

getAvgTemp :: [Measuring] -> Float
getAvgTemp ms = totalTemp / countMs
 where
    (totalTemp, countMs) = foldl (\ (total, count) (Temp _ temp) -> (total + temp, count + 1)) (0, 0) ms    -- `foldl` is better here than `foldl1` because it handles empty lists.

getClosestMeasuring :: [Measuring] -> Float -> Measuring
getClosestMeasuring ms avgTemp = foldl1 (\ m1 m2 -> if diff m1 < diff m2 then m1 else m2) ms                -- `foldr1` works here as well.
 where
    diff (Temp _ temp) = abs $ temp - avgTemp

getDay :: Measuring -> Int 
getDay (Temp day _) = day 