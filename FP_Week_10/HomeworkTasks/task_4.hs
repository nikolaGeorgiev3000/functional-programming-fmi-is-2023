                                                  -- Define a function that returns all the uncles of a node in a tree. An uncle is a brother of a parent.

main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findUncles t 10 == [5]
    -- print $ findUncles t 1 == []                -- -> `1` is the root (founder) of the tree 

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles t c = case getParent t c of             -- `c` === child, `p` === parent, `gp` === grandparent
    Nothing -> []                                 -- === noParent
    Just p -> case getParent t p of
        Nothing -> []                             -- === noGrandparent
        Just gp -> filter (/= p) (getChildren t gp)

getChildren :: Tree -> Int -> [Int]
getChildren t n = case lookup n t of              -- Look for the node `n` in the list pairs. If found, return the second el. of the found pair. 
    Nothing -> []
    Just cs -> cs                                 -- `cs` === list of children

getParent :: Tree -> Int -> Maybe Int
getParent t n = case filter (\ (_, cs) -> elem n cs) t of     
    [] -> Nothing
    (p, _) : _ -> Just p                          -- If a node has `>1` parents, we extract the first one (from left to right)
