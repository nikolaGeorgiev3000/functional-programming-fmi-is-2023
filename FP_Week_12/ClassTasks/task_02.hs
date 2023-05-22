-- number of nodes, for which the sum of their children is equal to the value of their parent
main :: IO()
main = do
    print $ numOfNodes t == 2 -- 8 and 12

data NTree a = Nil | Node a [NTree a]


numOfNodes :: (Num a, Eq a) => NTree a -> Int
numOfNodes Nil = 0
numOfNodes (Node grandParent parents) =
    length (filter (\ parent -> grandParent == sum (getLevel parent 1)) parents)
    + (sum $ map numOfNodes parents)

getLevel :: NTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _) 0 = [value]
getLevel (Node value children) k = concatMap (\ tree -> getLevel tree (k - 1)) children


t = Node 10[
    Node 3 [Node 5 [Nil], Node 8 [Node 1 [Nil], Node 2 [Nil]], Node 9 [Nil]],
    Node 7 [Node 11 [Nil], Node 13 [Nil]], 
    Node 12 [Node 6 [Nil], Node 4 [Nil]]]