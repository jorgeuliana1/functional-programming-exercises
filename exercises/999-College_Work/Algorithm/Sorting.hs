module Algorithm.Sorting where

{-
Ordenates a list of ordenable elements.
# Input
unorderedList :: [Ord] (Unordered list)
# Output
orderedList:: [Ord] (Ordered list)
-}
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (pivot:sequence) = smaller ++ [pivot] ++ bigger
    where
        smaller = quickSort $ filter (< pivot) sequence
        bigger  = quickSort $ filter (> pivot) sequence