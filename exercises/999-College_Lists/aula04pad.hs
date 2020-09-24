-- a. Calculate the factorial of a non-negative integer.
factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial (x - 1)

-- b. Calculate the product of two non-negative integers.
product :: Int -> Int -> Int
product x y = x * y

-- c. Calculate the power of two non-negative integers.
power :: Int -> Int -> Int
power a b = a^b

-- d. Calculate the n-esimal term of the Fibonacci series.
fibonacci :: Int -> Int
fibonacci x
    | x <  0 = 0
    | x == 0 = 1
    | x == 1 = 1
    | otherwise = fibonacci (x - 1) + fibonacci (x - 2)

-- e. Obtain the first element of a list
first :: [a] -> a
first (x:xs) = x

-- f. Take the first element of a list
firstLess :: [a] -> [a]
firstLess [] = []
firstLess (x:xs) = xs

-- g. Obtain the last element of a list
lastElem :: [a] -> a
lastElem xs = first (reverse xs)

-- h. Take the last element of a list
lastLess :: [a] -> [a]
lastLess [] = []
lastLess xs = reverse (firstLess (reverse xs))

-- i. Define an 'XOR' operator '|*|'
(|*|) :: Bool -> Bool -> Bool
(|*|) a b = (a /= b) && (b /= False || a /= False)