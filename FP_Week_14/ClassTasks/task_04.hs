import Data.List

main :: IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True 


isBoring :: (Eq a) => NTree a -> Bool
isBoring = (== 1) . length . nub . traverseDFS

traverseDFS :: NTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value children) = value : concatMap traverseDFS children -- concatMap (\ tree -> [value] ++ traverseDFS tree) children

data NTree a = Nil | Node a [NTree a]

t1 :: (Num a) => NTree a
t1 = Node 10 [Node 10 [Node 10 [Nil], Node 8 [Node 10 [Nil]], Node 2 [Nil]], Node 10 [Node 11 [Nil], Node 10 [Nil], Node 6 [Nil]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [Node 's' [Nil], Node 's' [Node 's' [Nil]]]]