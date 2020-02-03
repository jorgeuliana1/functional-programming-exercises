-- My quick sort algorithm in Haskell (most common approach)
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = [] -- In case the list is empty

-- Now, this x:xs is amazing!
-- If we input [a, b, c, d]
-- The function will understand x = a, xs = [b, c, d]
quickSort(x:xs) =
    -- Quicksort's partition right here
    let smaller = quickSort [ n | n <- xs, n <= x ]
        bigger  = quickSort [ n | n <- xs, n  > x ]

    -- Merging both partitions
    in  smaller ++ [x] ++ bigger

main = print(quickSort [1231, 12313, 41123, 124, 12, 123, 31, 2313])
-- outputs: [12,31,123,124,1231,2313,12313,41123]