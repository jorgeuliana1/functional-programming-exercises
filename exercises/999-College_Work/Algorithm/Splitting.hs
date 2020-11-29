module Algorithm.Splitting (kFold) where

{-
Generates a list of indexes of a specific sub-interval.
# Input
iterationNum :: Int (Iteration where the sub-interval will live)
subIntervalsLength :: Int (Length of the sub-interval)
# Ouput
subInterval :: [Int] (Sub-interval indexes)
-}
defineSubInterval :: Int -> Int -> [Int]
defineSubInterval iterationNum subIntervalsLength = [ index | index <- [startingIndex .. endingIndex] ]
    where
        startingIndex = iterationNum * subIntervalsLength
        endingIndex = (iterationNum + 1) * (subIntervalsLength) - 1

{-
Generates a list of sub-intervals indexes given the length of the interval and the number of sub-intervals.
# Input
numIntervals :: Int (Number of sub-intervals to be generated)
listLength :: Int (The length of the original interval)
# Output
subIntervalsIndexes :: [[Int]] (List of lists of indexes of each sub-interval)
-}
generateSubIntervals :: Int -> Int -> [[Int]]
generateSubIntervals numIntervals listLength =
    (zipWith (++) firstSubIntervals restingSubInterval) ++ lastSubIntervals
    where
        restingSubInterval = [ [i] | i <- [(listLength - r)..listLength] ]
        lastSubIntervals   = [ defineSubInterval (i - 1) l | i <- [(r + 1)..numIntervals]]
        firstSubIntervals  = [ defineSubInterval (i - 1) l | i <- [1..r] ]
        l = listLength `div` numIntervals
        r = listLength `mod` numIntervals

{-
Folds a list in k (given) sets.
# Input
k :: Int (The number of folds to be performed)
list :: [a] (Original list to be folded)
# Output
foldedIndexes :: [[a]] (Folded list)
-}
kFold :: Int -> [a] -> [[a]]
kFold k list = [ [ list !! i | i <- subInterval ] | subInterval <- generateSubIntervals k $ length list ]