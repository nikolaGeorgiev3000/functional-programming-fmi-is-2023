-- Growing plant

main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910

growingPlant :: Double -> Double -> Double -> Int
growingPlant upSpeed downSpeed desiredHeight 
 | upSpeed >= desiredHeight = 1 -- Always going to take less than or equal to a full day in this case
 | otherwise = 1 + ceiling ((desiredHeight - upSpeed) / (upSpeed - downSpeed)) -- Neat formula I found online