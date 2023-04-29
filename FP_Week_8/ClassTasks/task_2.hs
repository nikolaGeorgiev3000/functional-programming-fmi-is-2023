-- A function which checks whether a given path is valid 

main :: IO()
main = do
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

getChildren :: Graph -> Node -> [Node]
getChildren g n = head [children | (parent, children) <- g, parent == n] -- children is a list -> we get a list of lists, take the head, and the result is a list

isPath :: Graph -> Path -> Bool
isPath g path = all (\ (parent, child) -> elem child (getChildren g parent)) $ zip path (tail path)
-- Explanation: We turn the path into a list of pairs -> Ex: zip [1, 3, 4] [3, 4] == [(1, 3), (3, 4)]. Then we check whether all elements of the list satisfy the
-- predicate that the second element of the pair is amongst the list of children of the first element of the pair.
