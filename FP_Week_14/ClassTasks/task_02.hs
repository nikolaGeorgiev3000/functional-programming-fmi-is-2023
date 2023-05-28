import Data.List

main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False


type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)

data Attendance = Absent | Late | Present
 deriving (Eq)
type StudentRecord = [Attendance]

cP = canPass (1, 2)

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (misses, lates) = (\ records -> lessThanMisses records && lessThanLates records)
 where
    lessThanMisses = (<= misses) . length . filter (== Absent)                          -- The list of [Absent] should be <= the number of `misses`.
    lessThanLates  = null . filter (\ ys -> elem Late ys && length ys > lates) . group  -- If there exists a non-empty sublist with a length, bigger than the number `lates`, then the student cannot pass.