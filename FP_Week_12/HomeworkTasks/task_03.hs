{-
Define a function that returns the depth of the deepest blue node.
-}
main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2
    -- print $ maxDepthBlueNode Empty


maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode t = getDepth t (maxDepth t)

maxDepth :: Tree -> Int
maxDepth Empty               = 0
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)

getDepth :: Tree -> Int -> Int
getDepth Empty _   = error "The tree does not contain a 'Blue' node."
getDepth _     0   = 0
getDepth t startLvl = helper (getLevel t startLvl) startLvl
 where
    helper :: [Color] -> Int -> Int
    helper colors lvl
     | elem Blue colors = lvl
     | otherwise        = getDepth t (lvl - 1)
     
getLevel :: Tree -> Int -> [Color]
getLevel Empty _                   = []
getLevel (Node color _ _) 0        = [color]
getLevel (Node color left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)


data Color = Red | Green | Blue
 deriving (Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Green (Node Red (Node Green Empty Empty) Empty) 
                      (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)