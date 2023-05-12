{-
Let (x, y, z) be a vector representing the nodes of a binary tree such that x is the 
value of the current node, y and z are the values of the child nodes. 
Define a function that returns the leaves of such a tree.
-}
import Data.List (nub)

main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5], [3, 4, 5], order does not matter
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

type Node = Int
type Tree = [(Node, Node, Node)]


listLeaves :: Tree -> [Node]
listLeaves t = filter (`notElem` nodes) distinctNodes
 where
    nodes = map (\ (x, _, _) -> x) t 
    distinctNodes = nub $ concatMap (\ (x, y, z) -> [x, y, z]) t


