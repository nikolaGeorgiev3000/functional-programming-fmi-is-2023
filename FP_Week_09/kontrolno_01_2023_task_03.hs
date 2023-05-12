import Data.List

main :: IO()
main = do
    print $ numContentChildren [1, 2, 3] [1, 1] == 1
    print $ numContentChildren [1, 2] [1, 2, 3] == 2
    print $ numContentChildren [3,3,3,3,3,3,3] [17] == 1
    print $ numContentChildren [22, 1, 211] [17, 3, 24, 332] == 3

numContentChildren :: [Int] -> [Int] -> Int
numContentChildren gs ss = helper (sort gs) (sort ss) -- Sorting the lists in order to map children-bisq correctly
 where
    helper :: [Int] -> [Int] -> Int
    helper (g:gs) (s:ss)
     | g <= s = 1 + helper gs ss                      -- `ss` because this bisquit is eaten by/given to this child :D 
     | otherwise = helper (g:gs) ss
    helper _ _ = 0                                    -- Used to handle cases where we can't split head-tail.