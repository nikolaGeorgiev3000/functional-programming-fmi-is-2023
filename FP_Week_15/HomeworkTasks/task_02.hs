import Data.List

main :: IO()
main = do
    print $ stocklist stocks ['A', 'B'] == [('A', 200),('B', 1140)]
    print $ stocklist stocks ['C', 'X'] == [('C', 500),('X', 0)]
    print $ stocklist stocks ['Y', 'X'] == [('Y', 0),('X', 0)]
    print $ stocklist stocks ['C']      == [('C', 500)]


stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _ = []
stocklist _ [] = []
stocklist stocksInfo categories = [(capCategory, sumCountBooks capCategory groupedBooks) | capCategory <- categories]
 where
    groupedBooks = groupBooks booksInfo
    booksInfo    = extractStocksData stocksInfo
    
-- Extract only the necessary data
extractStocksData :: [Stock] -> [(Char, Int)]
extractStocksData [] = []
extractStocksData (Stock str countBooks : rest) = (head str, countBooks) : extractStocksData rest

-- Group the books data by a category
groupBooks :: [(Char, Int)] -> [(Char, [Int])]
groupBooks stocksData = nub [(capCategory, filterByCategory capCategory stocksData) | (capCategory, _) <- stocksData]

-- Sum the count of books given a category
sumCountBooks :: Char -> [(Char, [Int])] -> Int
sumCountBooks capCategory groupedData = sum $ concat $ filterByCategory capCategory groupedData

-- Eliminate code repetition
filterByCategory :: (Eq a) => a -> [(a, b)] -> [b]
filterByCategory c xs = [y | (x, y) <- xs, x == c]
 
-- Alg. type & Database
data Stock = Stock String Int 

stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]