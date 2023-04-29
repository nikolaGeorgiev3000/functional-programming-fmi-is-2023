
main :: IO()
main = do
    print $ dfs "a" "i" graph
    print $ bfs "a" "i" graph 



type Node = String
type Graph = [(Node,[Node])]
type Path = [Node]

graph :: Graph
graph = [("a",["b","c","d"]),("b",["e","f"]),
         ("c",["g","i"]),("d",["f","h"]),
         ("e",["i"]),("f",["j"]),("h",["j"])]

assoc :: Eq a => a -> [(a,[b])] -> (a,[b])
assoc key [] = (key,[])
assoc key (x:xs)
  | fst x == key = x
  | otherwise  = assoc key xs

succs :: Node -> Graph -> [Node]
succs n g = snd $ assoc n g



-- Extend the frontier
extend :: Path -> Graph -> [Path]
extend p g = concatMap (\ x -> if (elem x p) then [] else [x:p])
       $ succs (head p) g



-- Depth First Search
df :: [Path] -> Node -> Graph -> Path
df [] _ _ = []
df (p:leftover) goal g
 | goal == head p = p
 | otherwise = df ((extend p g) ++ leftover) goal g

dfs :: Node -> Node -> Graph -> Path
dfs n1 n2 g = reverse $ df [[n1]] n2 g
 
-- Breadth First Search
bf :: [Path] -> Node -> Graph -> Path
bf [] _ _ = []
bf (p:leftover) goal g 
 | goal == head p = p
 | otherwise = bf (leftover ++ (extend p g)) goal g

bfs :: Node -> Node -> Graph -> Path
bfs n1 n2 g = reverse $ bf [[n1]] n2 g