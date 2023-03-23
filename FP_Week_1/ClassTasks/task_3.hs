main :: IO()
main = do
    print $ factRec 11 == 39916800
    -- print $ factRec (-11) -- error: x was negative
    print $ factIter 11 == 39916800
    print $ factIter (-11) -- error: x was negative

factRec :: Int -> Int
factRec n 
 | n < 0 = error "negative"
 | n == 0 = 1
 | otherwise = n * factRec (n - 1)

factIter :: Int -> Int
factIter n 
 | n < 0 = error "negative"
 | otherwise = helper n 1
 where 
    helper 0 result = result
    helper leftover result = helper (leftover - 1) (leftover * result)