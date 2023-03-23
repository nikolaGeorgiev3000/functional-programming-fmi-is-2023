-- A crawling snail

main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1

snail :: Double -> Double -> Double -> Int
snail height upCrawl slideDown  
 | upCrawl >= height = 1
 | otherwise = 1 + ceiling ((height - upCrawl) / (upCrawl - slideDown)) -- Same formula applied here   