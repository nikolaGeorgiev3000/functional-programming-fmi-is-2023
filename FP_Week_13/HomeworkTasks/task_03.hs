{-
A binary tree is a cone if at every level the sum of the nodes is greater than than the sum at the previous level.

For a binary tree made up of whole numbers define the following functions:

1) a function that returns the sum of the nodes at level k;
2) a function that returns whether a tree is a cone.
-}

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True


cone :: BTree Int -> Bool
cone = checkLevels . getLevels 

getLevels :: BTree Int -> [[Int]]
getLevels Nil  = [[]]
getLevels tree = takeWhile (not . null) $ map (getLevel tree) [0 .. ]

checkLevels :: [[Int]] -> Bool
checkLevels [[]]  = True
checkLevels [[_]] = True 
checkLevels [parentLevel, childLevel] = sum parentLevel < sum childLevel
checkLevels (parentLevel : rest@(childLevel : restLevels)) = sum parentLevel < sum childLevel && checkLevels rest   


levelSum :: BTree Int -> Int -> Int
levelSum Nil _ = error "The empty tree does not have any nodes."
levelSum t   k = sum $ getLevel t k

getLevel :: BTree Int -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)


data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree :: BTree Int
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))