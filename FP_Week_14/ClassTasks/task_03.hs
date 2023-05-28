main :: IO()
main = do
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2


maxSumSubT :: (Ord a, Num a) => BTree a -> a
maxSumSubT NullT = 0
maxSumSubT t@(Node value left right) = maximum [sumTree t, sumTree left, sumTree right]

sumTree :: (Num a) => BTree a -> a 
sumTree NullT = 0
sumTree (Node value left right) = value + sumTree left + sumTree right


data BTree a = NullT | Node a (BTree a) (BTree a)

t1 :: (Num a) => BTree a
t1 = Node 3 (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)

t2 :: (Num a) => BTree a
t2 = Node (-3) (Node 0 NullT NullT) (Node 2 (Node 0 NullT NullT) NullT)