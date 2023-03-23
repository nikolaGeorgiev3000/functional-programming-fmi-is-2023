-- Sum of the last 3 members of a fancy sequence

main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

-- Using the 'map' function
findSumMap :: Int -> Int -> Int -> Int
findSumMap a b n = sum $ takeLast 3 sequence
 where
    -- sequence = map (\i -> a + sum (map (\j -> 2^j * vç√zb) [0..i])) [0..n-1]
    sequence = map (\i -> a + (sum $ map ((*b) . (2^)) [0 .. i])) [0..n-1]
    takeLast x = take x . reverse
{- THOROUGH explanation of the function -> We first start with 3 parameters: a, b and n. What we are looking for is the sum
of the last 3 terms in our sequence. Note that every term of our seq is defined as a sum of other terms. So, generally, we want
the sum of the last 3 terms of our sequence. Our map function takes two parameters - a function, and a list. The nested map 
function does the same. The outer map function iterates through the terms of our sequence, whilst the INNER map function
iterates through the additives, that form a specific term of the sequence. An analogy could be made with 2 nested for-loops from 
C++. The takeLast function reverses the sequence we got, and takes its first 3 elements (the last 3 elements of the original
non-reversed seq). Note, that the order of the additives (now, the terms of the seq) could be reversed again, in order to
preserve the original order. This is pointless in our program since the addition is a commutative process. Finally, we sum
these 3 terms. The trickiest part of the task was to realise that each term of the seq. is a sum of other terms, thus needing
a nested loop. -}

-- Using the recursive solution from Simo
findSum :: Int -> Int -> Int -> Int
findSum a b n = helper n + helper (n - 1) + helper (n - 2) -- Three additives, because the condition seeks for the "sum of the last three terms". NOTE: "helper n" accounts for the LAST TERM.
 where
    helper 0 = a -- The base case, in which the last additive (the first one in the "maths-way-of-writing-the-seq") is put
    helper n = 2^(n - 1) * b + helper (n - 1) -- Basically, reverse the order of the addition process. An analogy could be made
                                              -- with the definition of the factorial function: n! = n * fact (n - 1) = n * (n - 1)! = n * (n - 1) * (n - 2)! = ...
