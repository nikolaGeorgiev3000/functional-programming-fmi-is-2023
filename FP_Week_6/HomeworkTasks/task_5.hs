-- Points and lines

main :: IO()
main = do
    print $ onDiag (5.5, 5.5) == True
    print $ onDiag (0.5, 0) == False


type Point = (Double, Double)

roundToTwoDecimals :: Double -> Double 
roundToTwoDecimals num = (fromIntegral $ round $ num * 100) / 100

-- a)
line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = (\ x -> roundToTwoDecimals $ y1 + ((x - x1) * (y2 - y1)) / (x2 - x1))

-- b)
liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\ (x, y) -> y == f x)

-- Test cases "helpers"
diagonal = line (0,0) (1,1)
onDiag = liesOn diagonal