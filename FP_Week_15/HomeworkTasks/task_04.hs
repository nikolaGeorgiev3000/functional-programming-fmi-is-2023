main :: IO()
main = do
    print $ isPerfectlyBalanced t1 == True

data BTree a = Nil | Node a (BTree a) (BTree a)


isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced t = countNodes t == 2^(height t) - 1

countNodes :: BTree a -> Int
countNodes Nil = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

height :: BTree a -> Int
height Nil = 0
height (Node value left right) = 1 + max (height left) (height right)

t1 :: BTree Char
t1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))