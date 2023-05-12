main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"

-- Solution using `replicate`
-- repeater :: String -> (Int -> String -> String)
-- repeater str = (\ count glue -> concat (replicate (count - 1) (str ++ glue)) ++ str) 
-- Explanation: We use (count - 1) because we account for the last `str` with the `concat` function. 
-- Example: n = 4 -> replicate 3 (str ++ glue) -> str glue str glue str glue _ -> in the '_' place, we put the last `str` -> str glue str glue str glue str -> we finish with `str`

-- Solution using rec. call
repeater :: String -> (Int -> String -> String)
repeater str = (\ count glue -> if count == 1 then str else str ++ glue ++ (repeater str) (count - 1) glue)