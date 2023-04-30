import Data.List

main :: IO()
main = do
    -- print $ getMaxPair $ countOcc "184325998723"
    -- print $ (aroundFib 50) 5
    -- print $ (aroundFib 99) 25 == [('2', 4)]
    print $ (aroundFib 100) 25 == [('1',3)]
    print $ (aroundFib 180) 25 == [('1',5),('7',3)]
    print $ (aroundFib 1700) 25 == [('1',4),('2',5),('0',6),('4',5),('5',7),('2',4),('6',7),('3',5),('0',4),('8',5),('4',5),('4',4),('7',7),('7',6),('2',2)]
    print $ (aroundFib 500) 42 == [('0',6),('2',7),('2',6)]
    print $ (aroundFib 6000) 242 == [('5',31),('8',33),('8',31),('7',35),('7',31),('4',7)]


fibIter :: Integer -> Integer
fibIter n = helper 0 1 n
 where
    helper n0 n1 0 = n0
    helper n0 n1 1 = n1
    helper n0 n1 leftover = helper n1 (n0 + n1) (leftover - 1)

countOcc :: String -> [(Char, Int)]
countOcc xs = map (\ ys -> (head ys, length ys)) $ group $ sort xs      -- Sort, group into distinct characters -> return (Char, Occ/Length)

getMaxPair :: [(Char, Int)] -> (Char, Int)
getMaxPair gs = head [pair | pair@(num, freq) <- gs, freq == maxFreq]   -- Get the first pair with maxFreq
 where
    maxFreq = maximum $ map snd gs                                      -- maxLength of the occurrences

breakDown :: Int -> String -> [String]
breakDown k leftover
 | k > length leftover = [leftover]                                     -- When `k` is too big (also serving as base case), we have only one list of the list
 | otherwise = take k leftover : breakDown k (drop k leftover)          -- Take the first `k` chars, call rec. with the rest

aroundFib :: Integer -> (Int -> [(Char, Int)])
aroundFib n = (\ k -> map (\ gs -> getMaxPair $ countOcc gs) $ breakDown k (show $ fibIter n)) 
-- Get the n-th fib. number, convert it to a list, break it down into list of lists.
-- For each sublist, return a list of pairs [(Char, Freq)], and get the first pair from left to right
-- with the biggest number of occ. of the character. Remember, we are doing that for each of the sublist (map).