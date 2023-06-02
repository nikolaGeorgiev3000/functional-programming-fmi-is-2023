-- n is a positive whole number, xs is a list of numbers
import Data.Char

main :: IO()
main = do
    -- a)
    print $ digits 4321 == [4, 3, 2, 1]
    -- b)
    print $ decreasing [4, 3, 2, 1] == True
    print $ decreasing [4, 3, 5, 1] == False
    print $ decreasing [4, 3, 3, 1] == False 
    -- c)
    print $ decDigits 4321 == True 
    print $ decDigits 4322 == False   
    print $ decDigits 7635 == False

decDigits :: Int -> Bool
decDigits = decreasing . digits 

decreasing :: (Ord a, Num a) => [a] -> Bool
decreasing []  = True
decreasing [_] = True 
decreasing (x : rest@(y : zs)) = x > y && decreasing rest 

digits :: Int -> [Int]
digits = map digitToInt . show 