module VectorMath where

{-
Sum vectors:
Sums every value of a list of vectors and returns the resulting vector.
# Input
originvectors :: [[a]] (List of vectors to perform the sum)
# Output
resultingvector :: [a] (The resulting vector, with the sums of
                                each "index" of the vector)
-}
addVectors :: (Num a) => [a] -> [a] -> [a]
addVectors [] vectorB = vectorB
addVectors vectorA [] = vectorA
addVectors vectorA vectorB = [ (vectorA !! i) + (vectorB !! i) | i <- [0..(length vectorA) - 1] ]

sumVectors :: (Num a) => [[a]] -> [a]
sumVectors [] = []
sumVectors (componentA:vector) = componentA `addVectors` (sumVectors vector)

{-
Returns the sum of the elements of the vector.
# Input
vector :: [a] (List of vectors to perform the sum)
# Output
result :: a (The result of the sum of elements of the vector)
-}
sumElements :: (Num a) => [a] -> a
sumElements vector = sum vector

{-
Works as "for-each" division for vectors.
# Input
vector :: [a] (vector to be divided)
divisor :: a (Divisor of the elements of the vector)
# Output
resultingvector :: [a] (The resulting vector, with each element
                        of the original vector divided by the
                        given divisor)
-}
(////) :: (Fractional a) => [a] -> a -> [a]
vector //// divisor = [ value / divisor | value <- vector ]

{-
Works as "for-each" power function for vectors.
# Input
vector :: [a] (vector to be powered)
power :: Int (The power of the operation)
# Ouput
resultingvector :: [a] (The resulting vector)
-}
(^^^^) :: (Num a) => [a] -> Int -> [a]
vector ^^^^ power = [ v ^ power | v <- vector]

{-
Works as "for-each" sqrt function for vectors.
# Input
vector :: [a] (vector to be square-rooted)
# Ouput
resultingvector :: [a] (The square-rooted vector)
-}
vectorSqrt :: (Floating a) => [a] -> [a]
vectorSqrt vector = [ sqrt v | v <- vector ]

{-
Works as "for-each" subtraction for vectors.
# Input
vectorA :: [a] (First member of the subtraction operation)
vectorB :: [a] (Second member of the subtraction operation)
# Ouput
resultingvector :: [a] (The result of the subtraction)
-}
vectorSubtraction :: (Num a) => [a] -> [a] -> [a]
vectorSubtraction vectorA vectorB = [ (vectorA !! i) - (vectorB !! i) | i <- [0..(length vectorA) - 1]]

{-
Works as "for-each" abs for vectors.
# Input
vector :: [a] (Input vector)
# Ouput
resultingvector :: [a] (A vector containing the absolute
                                    values of the input vector)
-}
vectorAbs :: (Num a) => [a] -> [a]
vectorAbs vector = [abs v | v <- vector]

{-
Classical euclidean distance implementation.
# Input
vectorA :: [a] (vector representing a vector in R4)
vectorB :: [a] (vector representing a vector in R4)
# Output
distance :: a (Distance between vectorA and vectorB)
-}
euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance vectorA vectorB =
    sqrt (sumElements ((vectorA `vectorSubtraction` vectorB) ^^^^ 2))
