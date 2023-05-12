-- Final grade

main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84

roundToTwoDigits :: Double -> Double
roundToTwoDigits x = (fromIntegral $ round $ x * 100) / 100 -- Multiply, then round, then toFloat, then divide. 
-- Example: 345.25242 -> 34525.242 -> 34525 -> 34525.0 -> 345.25

finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it
 | d < 3 || iz < 3 || it < 3 = 2.0 
 | otherwise = roundToTwoDigits o
  where
    d  = (d1 + d2 + d3) / 3
    kt = (kt1 + kt2) / 2
    kz = (kz1 + kz2) / 2
    tk = 0.25 * d + 0.375 * kt + 0.375 * kz
    o  = 0.5 * tk + 0.25 * it + 0.25 * iz