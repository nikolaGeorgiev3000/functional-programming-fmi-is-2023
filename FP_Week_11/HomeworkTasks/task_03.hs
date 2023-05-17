-- Ordered according to the relation `subinterval`

main :: IO()
main = do
    print $ ordered Nil
    print $ ordered t1 == True
    print $ ordered t2 == False


data Tree a = Nil | Node a (Tree a) (Tree a)

                                                                
ordered :: Tree (Int, Int) -> Bool                  
ordered t = isMonotone (>) (firstEls) && isMonotone (<) (secondEls) -- Modify the function to pass a diff. condition for an order.
 where
    listDFS   = traverseDFS t
    firstEls  = map fst listDFS
    secondEls = map snd listDFS
         
isMonotone :: (a -> a -> Bool) -> [a] -> Bool                       -- Meaning, is there a `relation` (`f`) between the consecutive elements.
isMonotone f xs = and $ zipWith f xs (tail xs)

traverseDFS :: Tree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right


t1 :: Tree (Int, Int)
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: Tree (Int, Int)
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (7, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))