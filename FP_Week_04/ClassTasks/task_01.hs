-- Returns True IFF a list has at least one element

main :: IO()
main = do
    print $ hasElementsPM [] == False
    print $ hasElementsPM [1, 2, 3] == True

    print $ hasElementsFunc [] == False
    print $ hasElementsFunc [1, 2, 3] == True

    print $ hasElementsButWithMagic [] == False
    print $ hasElementsButWithMagic [1, 2, 3] == True

-- With PM
hasElementsPM :: [Int] -> Bool
hasElementsPM [] = False
hasElementsPM _ = True 

-- With Functions
hasElementsFunc :: [Int] -> Bool
hasElementsFunc xs = not $ null xs -- null xs returns true IFF the list is empty -> we negate it

-- Func at func level
hasElementsButWithMagic :: [Int] -> Bool
hasElementsButWithMagic = not . null 