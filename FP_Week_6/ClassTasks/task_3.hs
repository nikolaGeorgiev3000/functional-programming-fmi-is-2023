-- Define a function on a functional level that takes a word and returns a list of tuples in 
-- the form (x, xCount) where for each letter x, xCount is the number of times it is present in the word. Ignore capitalization.
import Data.List
import Data.Char

main :: IO()
main = do
    print $ countOccurrences "Test" == [('e',1),('s',1),('t',2)]
    print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a',6),('c',3),('d',1),('e',4),('g',2),('h',2),('i',3),('l',4),('m',1),('n',3),('o',4),('r',5),('s',3),('t',4),('v',1),('w',1),('y',2)]

countOccurrences :: String -> [(Char, Int)]
countOccurrences = map (\ xs -> (head xs, length xs)) . group . sort . map toLower
-- Explanation: We cycle through every letter and convert the capital ones to lower ones, and leave the lower ones intact.
-- We then sort the letters according to their numerical repr., group them into list of lists, take the head of every sub-list
-- and its length, and add those tuples into a list, which is the final result.