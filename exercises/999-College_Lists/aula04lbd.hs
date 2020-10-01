-- a. Calculate the factorial of a non-negative integer.
factorial :: Integer -> Integer
factorial = \ x -> product [1..x]

-- b. Calculate the product of two non-negative integers.
prod :: Int -> Int -> Int
prod = \ x y -> x * y

-- c. Calculate the power of two non-negative integers.
power :: Int -> Int -> Int
power = \ a b -> a^b

-- f. Obtain the first element of a list
first :: [a] -> a
first = \ xs -> head xs

-- g. Take the first element of a list
firstLess :: [a] -> [a]
firstLess = \ xs -> tail xs

-- h. Obtain the first 'n' even elements of list
evens :: Int -> [Int]
evens n = (filter (\ n -> even n)) [0..n*2 - 1]

-- i. Determine the odd elements of a list
odds :: [Int] -> [Bool]
odds xs = (map (\ n -> odd n)) xs