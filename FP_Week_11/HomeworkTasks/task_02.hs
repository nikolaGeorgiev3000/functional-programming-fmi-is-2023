-- Define a function that returns the depth of the shallowest green node.

main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2


data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree


minDepthGreenNode :: Tree -> Int 
minDepthGreenNode Empty = 0
minDepthGreenNode (Node Green _ _) = 0
minDepthGreenNode (Node _ left right) = 1 + min (minDepthGreenNode left) (minDepthGreenNode right)


colorTree :: Tree 
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)