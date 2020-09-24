-- Problem 1: Return the absolute value of a given (n) number
myAbs :: Int -> Int
myAbs n
    | n >= 0 = n
    | otherwise = -n

-- Problem 2: Return the size of a given (xs) list.
conta :: [a] -> Int -> Int
conta xs tam
    | null xs = tam
    | otherwise = conta (tail xs) (tam + 1)
tam :: [a] -> Int
tam xs = conta xs 0