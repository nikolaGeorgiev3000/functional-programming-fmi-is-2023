{-
Define a function that accepts a graph, a whole number k and a node n. 
Return all the paths starting from n that are (k + 1) nodes long (or `k` edges long). 
IMO, the paths are `k + 1` nodes long, meaning in each of the simple paths, there are `k + 1` nodes (or `k` edges).
If the node is not present, throw an error.
-}

main :: IO()
main = do
    -- print $ simplePaths [(1, [2, 3, 1]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]

type Node = Int
type Path = [Node]
type Graph = [(Node, [Node])]

nodes :: Graph -> [Node]
nodes = map fst      

neighbours :: Graph -> Node -> [Node]
neighbours g n = [ y | (x, ys) <- g, x == n, y <- ys]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths g k n
 | k < 0               = error "Can't have a negative length."
 | notElem n (nodes g) = error "This node is not present in the graph."
simplePaths _ 0 n = [[n]]
simplePaths g k n = [ n:p | nbr <- neighbours g n, p <- helper g k nbr, notElem n p] -- `notElem n p` is used to handle graphs, in which there EXISTS a Cycle
  where 
    helper :: Graph -> Int -> Node -> [Path]
    helper _ 1 n = [[n]]
    helper g k n = [ n:p | nbr <- neighbours g n, p <- helper g (k - 1) nbr]