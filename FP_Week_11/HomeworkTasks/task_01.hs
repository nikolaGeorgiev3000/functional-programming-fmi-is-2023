main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3

    print $ average numberBTree == 16.22
    --print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))

{-
​​height (number of nodes along the longest branch);​
average - returns the average of the nodes (should work only for trees that store numbers in their nodes);
​​sumLeaves​ - returns the sum of the leaves (should work only for trees that store numbers in their nodes);
areEqual - checks whether two trees are identical;
setLevels - replaces the values in all nodes with the vector ("level", "value");
mirrorTree - returns the symmetric tree.
-}

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)


mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node value left right) = Node value (mirrorTree right) (mirrorTree left)

setLevels :: BTree a -> BTree (Int, a)
setLevels t = helper t 0
 where
    helper :: BTree a -> Int -> BTree (Int, a)
    helper Nil _ = Nil 
    helper (Node value left right) level = 
        let 
            newValue = (level, value)
            newLeft  = helper left (level + 1)
            newRight = helper right (level + 1)
        in
            Node newValue newLeft newRight
            
areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual Nil Nil = True 
areEqual _   Nil = False
areEqual Nil _   = False
areEqual (Node v1 l1 r1) (Node v2 l2 r2) 
 | v1 /= v2 = False
 | otherwise = (areEqual l1 l2) && (areEqual r1 r2)

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node value Nil Nil) = value
sumLeaves (Node value left right) = sumLeaves left + sumLeaves right

average :: (Num a, Integral a) => BTree a -> Double   
average Nil = 0
average t   = roundToTwoDigits $ fromIntegral (sumTree t) / fromIntegral (size t)

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a 
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

roundToTwoDigits :: Double -> Double
roundToTwoDigits x = (fromIntegral $ round $ x * 100) / 100

height :: BTree a -> Int 
height Nil = 0
height (Node value left right) = 1 + max (height left) (height right)


numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))