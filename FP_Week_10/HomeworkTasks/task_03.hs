-- Define a function that accepts a list of points and returns a vector (Double, Point, Point) 
-- that represents the closest distance between any two points, and the points themselves.
import Data.List (minimumBy)
import Data.Ord (compare)

main :: IO()
main = do 
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)

data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq, Read) 

getClosestDistance :: (Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance [] = error "Empty list of points."
getClosestDistance ps = minimumBy compareDistances [(distance p1 p2, p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]
 where
    compareDistances (d1, _, _) (d2, _, _) = compare d1 d2 -- "LT", "GT" or "EQ" results from the `compare` func
    distance (TwoD x1 y1) (TwoD x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
    distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2
