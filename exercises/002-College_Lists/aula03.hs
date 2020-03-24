-- a. Function to find the biggest between two numbers.
biggest :: Integer -> Integer -> Integer
biggest a b = (if a > b then a else b)

-- b. Function to calculate the average of grades of an student.
averageGrade :: [Float] -> Float
averageGrade xs = (sum xs) / fromIntegral(length xs)

-- c. Function to verify if a student has passed the semester (average >= 7.00)
studentPassed :: [Float] -> Bool
studentPassed xs = (if (averageGrade xs) >= 7 then True else False)

-- d. Indicate if two numbers are crescent, decrescent or equal
orderType :: Float -> Float -> String
orderType a b = (if a > b then "Decrescent" else if a < b then "Crescent" else "Equal")

-- e. Find the biggest of three numbers
biggestOfList :: [Int] -> Int
biggestOfList [x] = x
biggestOfList (x:xs) = (if x >= (biggestOfList xs) then x else (biggestOfList xs))

biggestOf3 :: Int -> Int -> Int -> Int
biggestOf3 a b c = biggestOfList [a, b, c]

-- f1. Find out if three numbers can form a tringle
canFormTriangle :: Float -> Float -> Float -> Bool
canFormTriangle a b c = (if (a + b + c) == 180 then True else False)

-- f2. Find out what kind of tringle the three numbers will form
kindOfTriangle :: Float -> Float -> Float -> String
kindOfTriangle a b c =
    if not (canFormTriangle a b c)
        then "None"
        else if (a == b) && (b == c)
            then "Equilateral"
            else if (a == b) || (b == c) || (c == a)
                then "Isosceles"
                else
                    "Scalene"

-- g. To calculate the factorial of a number
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- h. Calculate the Greatest Commom Divisor
greatestCD :: Int -> Int -> Int
greatestCD a b = maximum [ x | x <- [1..c], a `mod` x == 0, b `mod` x == 0 ]
                 where
                     c = (if a > b then b else a)

-- i. Count the number of digits of an Integer
numberOfDigits :: Int -> Int
numberOfDigits x = if x >= 10 then (1 + numberOfDigits (x `div` 10)) else 1

-- j. Determine if an Integer is a palindrome
getLast :: Int -> Int
getLast x = if x >= 10 then (getLast (x `mod` 10)) else x

numToList :: Int -> [Int]
numToList x = reverse [ getLast (x `div` 10^n) | n <- [ 0..(numberOfDigits x - 1 ) ] ]

numIsPalindrome :: Int -> Bool
numIsPalindrome x = if (numToList x) == reverse (numToList x) then True else False

-- k. Convert a binary number to a decimal number
binaryToDecimal :: Int -> Int
binaryToDecimal 1 = 1
binaryToDecimal 0 = 0
binaryToDecimal x = sum [ (xs !! n)*2^n | n <- [0..length xs - 1] ]
                    where xs = reverse (numToList x)

-- l. Convert a decimal number to a binary number
decimalToBinary :: Int -> [Int]
decimalToBinary 1 = [ 1 ]
decimalToBinary 0 = [ 0 ]
decimalToBinary x = reverse ([ x `mod` 2 ] ++ reverse(decimalToBinary (x `div` 2)))

-- k. Create a safetail function that doesn't break when applied to empty list
safetail :: [t] -> [t]
safetail [] = []
safetail (x:xs) = xs
