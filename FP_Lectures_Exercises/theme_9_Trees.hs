
main :: IO()
main = do


type Node = Int
type Tree = [(Node, [Node])]


assoc :: Eq a => a -> [(a,[b])] -> (a,[b])
assoc key [] = (key, [])
assoc key (x:xs)
  | fst x == key = x
  | otherwise = assoc key xs

succs :: Node -> Tree -> [Node]
succs n t = snd $ assoc n t

isNode :: Node -> Tree -> Bool
isNode n t = rt n || lf n
 where 
    rt x = elem x $ map fst tree       -- Either a root
    lf x = elem x $ concatMap snd tree -- Or a leaf

isLeaf :: Node -> Tree -> Bool
isLeaf n t = isNode n t && null $ succs n t -- A node AND an empty list of successors (sons)

parent :: Node -> Tree -> Node
parent n [] = " "
parent n t
 | elem n (succs fn t) = fn
 | otherwise = parent n $ tail t
  where 
    fn = fst $ head t

root :: Tree -> Node
root t
  | parent fn t == " " = fn     -- `fn` stands for firstNode
  | otherwise = root $ tail t
   where 
    fn = fst $ head t

listOfLeaves :: Node -> Treetd -> [Node]
listOfLeaves node t
  | isLeaf n t = [n]
  | otherwise  = concatMap (\ x -> listOfLeaves x t) $ succs n t