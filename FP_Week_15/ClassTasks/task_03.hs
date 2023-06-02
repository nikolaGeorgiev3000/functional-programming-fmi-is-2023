main :: IO()
main = do
    print $ (averageFunction [(+1), (**0.5), (2**)]) 2 == 2.804738

averageFunction :: (Num a, Fractional a, RealFrac a) => [(a -> a)] -> (a -> a)
averageFunction fs = (\ x -> roundSixDig $ average $ map (\ fi -> fi x) fs)

average xs = sum xs / (fromIntegral $ length xs)

roundSixDig :: (Num a, Fractional a, RealFrac a) => a -> a 
roundSixDig = (/ 1000000) . fromIntegral . round . (* 1000000)