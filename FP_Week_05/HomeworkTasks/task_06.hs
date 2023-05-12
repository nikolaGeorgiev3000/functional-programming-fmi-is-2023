main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8

pairCompose :: [(Int -> Int)] -> (Int -> Int) -- List of functions as arguments. Return a function.
pairCompose [] = id
pairCompose [f] = f
pairCompose (f:g:fs) = (\x -> (f $ g x) + (pairCompose fs) x) -- Split the list of functions with `two heads`, apply the `second head` first, then the head
                                                              -- to that result. Add that to the recursive call of the rest of the list. The base cases are 
                                                              -- self-explanatory.