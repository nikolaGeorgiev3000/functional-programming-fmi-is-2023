{-
By using the Shape data type, define two functions that accept a list of shapes and:

1. The first returns their areas;
2. The second returns the shape with the biggest area. !! SOLVE USING FOLDING !!
-}

main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a 
 deriving (Eq) -- `Eq` functionality for the maxArea function

area :: (Floating a) => Shape a -> a 
area (Circle r) = pi * r * r 
area (Rectangle x y) = x * y 
area (Triangle x y z) = let halfP = (x + y + z) / 2.0 in sqrt $ halfP * (halfP - x) * (halfP - y) * (halfP - z) 
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r 

getAreas :: (Floating a) => [Shape a] -> [a]
getAreas = map (\ shape -> area shape) 

maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a -- Ord used for `>=`
maxArea = foldr1 (\ s1 s2 -> if area s1 >= area s2 then s1 else s2) 
