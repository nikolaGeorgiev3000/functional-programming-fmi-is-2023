
main :: IO()
main = do
   -- print $ (aroundFib 100) 25 == [('1',3)]
   -- print $ (aroundFib 180) 25 == [('1',5),('7',3)]
   -- print $ (aroundFib 1700) 25 == [('1',4),('2',5),('0',6),('4',5),('5',7),('2',4),('6',7),('3',5),('0',4),('8',5),('4',5),('4',4),('7',7),('7',6),('2',2)] 
   -- print $ (aroundFib 500) 42 → [('0',6),('2',7),('2',6)] 
   -- print $ (aroundFib 6000) 242 == [('5',31),('8',33),('8',31),('7',35),('7',31),('4',7)]


fibIter :: Int -> Integer -- Use Integer in order to allow bigger values
fibIter n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = helper 0 1 1 n 
     where helper x y curr upperBound
            | curr >= upperBound = y
            | otherwise = helper y (x + y) (curr + 1) upperBound

