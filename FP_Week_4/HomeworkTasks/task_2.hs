-- Whether an element is present in a list

main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True   

-- Solve using a linearly recursive process WITHOUT pattern matching.
isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs = (not $ null xs) && (n == head xs || isPresentRecNonPM n (tail xs))

-- Solve using a linearly recursive process WITH pattern matching.
isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False 
isPresentRecPM n (x:xs) = n == x || isPresentRecPM n xs

-- Solve using functions that work with lists.
isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc n = elem n 
