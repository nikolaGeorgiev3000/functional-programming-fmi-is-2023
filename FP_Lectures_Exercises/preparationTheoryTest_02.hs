main :: IO()
main = do
    print $ roundTwoDigit 2.152155
    print $ (higherOrderEx [3.2, 5.3]) 5
    

    print $ lazyEv2 (43 * 17) (lazyEv2 1351 5332)

    -- As much is necessary:
    print $ take 5 [9, 1532 .. ]


roundTwoDigit :: Double -> Double
roundTwoDigit = (/ 100) . fromIntegral . round . (* 100)

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (x:xs)
--  | p x       = x : filter p xs
--  | otherwise = filter p xs

higherOrderEx :: [Double] -> (Int -> Int)
higherOrderEx xs = (\ scalar -> scalar * (round $ maximum xs))  

-- Lazy evaluation
-- Example 1 -> Arguments, which are not needed, are not being evaluated.
f :: Float -> Float -> Float
f x y = x + 10

g :: Float -> Float 
g x = 5 + g x 

lazyEv1 = f 5 (g 3)

-- Example 2
lazyEv2 :: Int -> Int -> Int
lazyEv2 x y = x * x