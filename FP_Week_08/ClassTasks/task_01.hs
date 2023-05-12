import Data.List (nub)


main :: IO()
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] == [1, 2, 3, 4]

    print $ neighbours 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbours 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []

    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

type Node = Int     
type Edge = (Node, Node)
type Graph = [Edge]

neighbours :: Node -> Graph -> [Node]
neighbours n g = [ y | (x, y) <- g, x == n] -- We force the first element of the pair to be the node `n`

nodes :: Graph -> [Node]
nodes = nub . concatMap (\ (x, y) -> [x, y]) -- Take the edges as arguments of the lambda function, and return a list of lists, then concat them into a single list, and remove the duplicates

adjacencyList :: Graph -> [(Node, [Node])]
adjacencyList g = [(n, neighbours n g) | n <- nodes g] 