-- Return a list of volumes

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)]  == [785.4, 157.08, 125.66, 62.83]

type Cylinder = (Double, Double)


roundToTwoDecimals :: Double -> Double -- This function is used to match the provided test
roundToTwoDecimals num = (fromIntegral $ round $ num * 100) / 100

getVolumes :: [Cylinder] -> [Double]
getVolumes cylinders = map (\ (r, h) -> roundToTwoDecimals $ pi * r * r * h) cylinders