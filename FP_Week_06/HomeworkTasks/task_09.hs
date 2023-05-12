-- Define a function that accepts a string and removes all duplicate letters
-- Two characters are duplicate, if: 
-- They represent the same character
-- AND they are next to each other
-- AND the first is upper-case and the second - lower-case (or vice versa).
import Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD 
                                                         -- Note: "DD" are not removed since they do not satisfy the third condition
areDups :: Char -> Char -> Bool
areDups x y = toLower x == toLower y && isUpper x /= isUpper y

-- NOTE: Solving the task with lin. iterative process, using an accumulator, and reversing it, in order to manage with "aAa", "FfF", etc...
reduceStr :: String -> String
reduceStr = reverse . helper []
 where 
    helper :: String -> String -> String
    helper acc [] = acc
    helper [] (x:xs) = helper [x] xs
    helper (s:ss) (x:xs)
     | areDups s x = helper ss xs -- Skip the dubs
     | otherwise = helper (x:s:ss) xs          