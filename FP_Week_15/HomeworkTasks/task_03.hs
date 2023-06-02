main :: IO()
main = do
    print $ matching "1234" == []
    print $ matching ",[.[-],]" --  == [(3, 5), (1, 7)] -- || [(1, 7), (3, 5)]
    print $ matching ",+[-.,+]" == [(2, 7)]
    print $ matching "[][]" == [(0, 1), (2, 3)]


matching :: String -> [(Int, Int)]
matching str = findMatches str [] 0

findMatches :: String -> [(Int, Int)] -> Int -> [(Int, Int)]
findMatches [] stack _ = stack                                                     -- `stack` === `result`
findMatches (x : xs) stack i                                                       -- `i` === current index
 | x == '[' = findMatches xs (stack ++ [(i, findClosingBracket xs (i + 1) 0)]) (i + 1)
 | otherwise = findMatches xs stack (i + 1)

findClosingBracket :: String -> Int -> Int -> Int
findClosingBracket [] _ 0 = error "No matching closing bracket found."
findClosingBracket (x : xs) i count
 | x == '['  = findClosingBracket xs (i + 1) (count + 1)                           -- Case of nested brackets.
 | x == ']'  = if count == 0 then i else findClosingBracket xs (i + 1) (count - 1) -- Depends whether we're still inside nested brackets.
 | otherwise = findClosingBracket xs (i + 1) count                                 -- Skip the current character. 