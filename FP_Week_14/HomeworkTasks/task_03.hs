import Data.List 

main :: IO()
main = do
    print $ isPrimeDictionary t1 vocabulary == False
    print $ isPrimeDictionary t2 vocabulary == False
    print $ isPrimeDictionary t3 vocabulary == True

    
-- Primary function section
isPrimeDictionary :: BTree -> Vocabulary -> Bool
isPrimeDictionary Nil _ = False
isPrimeDictionary t voc = isPrime $ sum [getSubstrLenSum dictWord treeWords | dictWord <- voc] -- For each word from our vocabulary, calculate the `smaller sum`.
 where
    treeWords = getWords t


-- Generate tree level-words section
getWords :: BTree -> [String]                -- Returns a list of strings, where string_i repr. the word at the level_(i - 1).
getWords t = [concat $ getLevelLetters t k | k <- [0 .. maxLevel]]
 where
    maxLevel = maximum $ getTreeLevels t

getTreeLevels :: BTree -> [Int]              -- Returns a list of all levels of the tree, starting from the 0-th (root) level.
getTreeLevels t = nub $ helper t 0
 where
    helper Nil _ = []
    helper (Node _ left right) lvl = lvl : helper left (lvl + 1) ++ helper right (lvl + 1)

getLevelLetters :: BTree -> Int -> [String]  -- Returns a list of letters (repr. as String) at level `k`.
getLevelLetters Nil _ = []
getLevelLetters (Node value left right) 0 = [[value]]
getLevelLetters (Node value left right) k = getLevelLetters left (k - 1) ++ getLevelLetters right (k - 1)


-- Calculate `smaller sums` section
getSubstrLenSum :: String -> [String] -> Int -- Calculate a `smaller sum`, where `dictWord` is fixed.
getSubstrLenSum dictWord treeWords = sum [level + getLenOcc dictWord treeWord | (level, treeWord) <- zip [0..] treeWords, getLenOcc dictWord treeWord > 0]

getLenOcc :: String -> String -> Int
getLenOcc dictWord treeWord = sum [length dictWord | substr <- getSubstrs treeWord, isPrefixOf dictWord substr]

getSubstrs :: String -> [String]
getSubstrs [] = []
getSubstrs xs@(_ : rest) = xs : getSubstrs rest


-- isPrime function
isPrime :: Int -> Bool
isPrime x = null [d | d <- [2 .. x - 1], mod x d == 0]


-- Database section
type Vocabulary = [String]

data BTree = Nil | Node Char BTree BTree
 deriving (Show)


vocabulary :: Vocabulary
vocabulary = ["the", "a", "Some", "swimming", "liStS", "lisp"]


t1 :: BTree
t1 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) 
                        (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil)))
              (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 'S' Nil Nil)) 
                        (Node 'a' (Node 't' Nil Nil) (Node 'S' Nil Nil)))

t2 :: BTree
t2 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) 
                        (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil)))
              (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 's' Nil Nil)) 
                        (Node 'p' (Node 'p' Nil Nil) (Node 'S' Nil Nil)))    

t3 :: BTree
t3 = Node 'a' (Node 't' (Node 'l' Nil Nil) (Node 'i' Nil Nil)) 
              (Node 'h' (Node 's' Nil Nil) (Node 'p' Nil Nil))