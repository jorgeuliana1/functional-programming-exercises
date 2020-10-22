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

-- j. Create a function using sections to obtain the inverse of a number
inverse :: (Fractional a) => a -> a
inverse = (1/)

-- k. Create a function using sections to obtain the double of a number
doubleNum :: (Fractional a) => a -> a
doubleNum = (2*)

-- l. Create a function using sections to obtain the half of a number
halfNum :: (Fractional a) => a -> a
halfNum = (/2)

-- n. Create a function using sections to obtain the cube of a number
cubedNum :: (Num a) => a -> a
cubedNum = (^3)

-- o. Create a function using sections to obtain the n-esimal power of 10
powerOf10 :: (Integral a) => a -> a
powerOf10 = (10^)

-- q. Create a function using sections to obtain the antecessor of a number
antecessor :: (Num a) => a -> a
antecessor = (subtract 1)

-- r. Create a function using sections to obtain the first n multiples of 3
multiplesOf3 :: Int -> [Int]
multiplesOf3 n = (map (*3)) [1..n]

