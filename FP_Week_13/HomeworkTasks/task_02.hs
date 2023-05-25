main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))


convert :: (Num a, Ord a) => BTree a -> BTree a
convert t = helper t (traverseDFS t)                  -- Pass the tree and its mon. incr. sequence.
 where
    helper :: (Num a, Ord a) => BTree a -> [a] -> BTree a
    helper Nil                   _       = Nil
    helper (Node val left right) seqList = Node sumGreater newLeft newRight
     where 
        sumGreater = sum $ dropWhile (< val) seqList  -- Calculate the sum of values greater than or equal to current value.
        newLeft    = helper left  seqList                 
        newRight   = helper right seqList 

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []   
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Eq)

tree :: (Num a) => BTree a
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))