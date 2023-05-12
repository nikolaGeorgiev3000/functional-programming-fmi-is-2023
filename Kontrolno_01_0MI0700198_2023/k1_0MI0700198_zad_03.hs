-- The first argument is a list gs with the children's wishes
-- The second one is a list ss with the actual sizes of the bisq


main :: IO()
main = do
    print $ numContentChildren [1, 2, 3] [1, 1] == 1 -- 3 children, 2 bisq with according sizes [1, 1] -> only the first children with size pref >= 1 will be satisifed
    print $ numContentChildren [1, 2] [1, 2, 3] == 2 -- 2 children, 3 bisq with acc. sizes [1, 2, 3] -> both children will be sat. since there are bisq with bigger than or eq sizes
    print $ numContentChildren [3,3,3,3,3,3,3] [17] == 1
    print $ numContentChildren [22, 1, 211] [17, 3, 24, 332] == 3
-- Solution idea:
-- We fix the element g[i] of the  list, then we check the elements of the second list and compare whether there exists an element s[j] >= g[i]

numContentChildren :: [Int] -> [Int] -> Int
numContentChildren [] _ = 0
numContentChildren (g:gs) ss
 | or $ map (\ x -> g <= x) ss = 1 + numContentChildren gs ss -- the map function returns a [Bool]. We make a disjunction between all of the elements of that list .
 | otherwise = numContentChildren gs ss
