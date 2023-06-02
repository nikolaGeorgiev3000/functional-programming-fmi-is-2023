import Data.Char

main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == -811181
    
squareDigits :: Int -> Int
squareDigits 0 = 0
squareDigits n
 | n < 0     = negate $ squareDigits (-n)
 | otherwise = read $ concatMap (show . (^2) . digitToInt) $ show n  