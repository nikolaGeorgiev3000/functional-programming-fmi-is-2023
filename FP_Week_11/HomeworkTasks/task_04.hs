{-
For an algebraic type representing an n-ary tree, define a predicate that checks whether it is a graceful tree. 
We say that a tree is a graceful tree if the absolute difference between every child node and its father is an even number.
-}

main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False


data NTree a = Nil | Node a [NTree a]


isGraceful :: NTree Int -> Bool
isGraceful Nil = True
isGraceful (Node value children) = all (isGracefulNode value) children 

-- `isGracefulNode` is partially applied, "thanks" to the `all` function. The first argument (Int) is fixed to `value`, 
-- and the result is a function, that accepts a (NTree Int) argument and returns (Bool). 
-- The same approach is used in the recursive call in the `isGracefulNode` function.
-- Powerful tool, as we can fix one (or more) argument/s, and "cycle through" different options for the rest.

isGracefulNode :: Int -> NTree Int -> Bool
isGracefulNode _ Nil = True
isGracefulNode parentValue (Node value children) = isDiffEven parentValue value && all (isGracefulNode value) children 

isDiffEven :: Int -> Int -> Bool
isDiffEven x y = even $ abs $ x - y


t1 :: NTree Int 
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]
 
t2 :: NTree Int
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]