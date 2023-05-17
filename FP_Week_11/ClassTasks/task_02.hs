-- Define a recursive polymorphic algebraic type representing an n-ary tree. Create one instance and print it.

main :: IO()
main = do
    print $ nTreeExample

data NTree a = Nil | Node a [NTree a]
 deriving (Show)

nTreeExample :: NTree Int
nTreeExample = Node 5 [Node 4 [Nil], Node 3 [Nil], Node 2 [Nil], Node 1 [Nil]]