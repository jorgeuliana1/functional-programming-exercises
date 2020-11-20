module Math.Vector where

{-
This module defines useful types for vector manipulation.
The definition of a vector in this specific implementations is a list of numbers.
-}

data Vector a = Vector [a] deriving (Eq, Show)

{-
Length of the vector:
Gets the number of dimensions of the vector.
# Input
originvectors :: Vector a (The vector to be analyzed)
# Output
length :: Int (The length of the vector)
-}
lenVector :: Vector a -> Int
lenVector (Vector a) = length a

{-
Sum vectors:
Sums every value of a list of vectors and returns the resulting vector.
# Input
originvectors :: Vector [Num] (List of vectors to perform the sum)
# Output
resultingvector :: Vector Num (The resulting vector, with the sums of
                                each "index" of the vector)
-}
addVectors :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a) `addVectors` (Vector b) = Vector $ zipWith (+) a b

sumVectors :: (Num a) => [Vector a] -> Vector a
sumVectors vs = foldr (addVectors) nullVector vs
    where nullVector = Vector [0 | i <- [1..(lenVector $ head vs)]] -- generating a n-sized null vector.

{-
Returns the sum of the elements of the vector.
# Input
vector :: Vector Num (List of vectors to perform the sum)
# Output
result :: Num (The result of the sum of elements of the vector)
-}
sumElements :: (Num a) => Vector a -> a
sumElements (Vector v) = sum v

{-
Works as "for-each" subtraction for vectors.
# Input
vectorA :: Vector Num (First member of the subtraction operation)
vectorB :: Vector Num (Second member of the subtraction operation)
# Ouput
resultingvector :: Vector Num (The result of the subtraction)
-}
vectorSubtraction :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a) `vectorSubtraction` (Vector b) = Vector $ zipWith (-) a b

{-
Works as "for-each" sqrt function for vectors.
# Input
vector :: Vector Floating (vector to be square-rooted)
# Ouput
resultingvector :: Vector Floating (The square-rooted vector)
-}
vectorSqrt :: (Floating a) => Vector a -> Vector a
vectorSqrt (Vector a) = Vector $ map sqrt a

{-
Works as "for-each" abs for vectors.
# Input
vector :: Vector Num (Input vector)
# Ouput
resultingvector :: Vector Num (A vector containing the absolute
                                    values of the input vector)
-}
vectorAbs :: (Num a) => Vector a -> Vector a
vectorAbs (Vector a) = Vector $ map abs a

{-
Works as "for-each" division for vectors.
# Input
vector :: Vector Fractional (vector to be divided)
divisor :: Fractional (Divisor of the elements of the vector)
# Output
resultingvector :: [a] (The resulting vector, with each element
                        of the original vector divided by the
                        given divisor)
-}
(////) :: (Fractional a) => Vector a -> a -> Vector a
(Vector a) //// b = Vector $ map (\x -> x / b) a

{-
Works as "for-each" power function for vectors.
# Input
vector :: Vector Num (vector to be powered)
power :: Integral (The power of the operation)
# Ouput
resultingvector :: Vector Num (The resulting vector)
-}
(^^^^) :: (Num a, Integral b) => Vector a -> b -> Vector a
(Vector a) ^^^^ b = Vector $ map (\x -> x ^ b) a

{-
Classical euclidean distance implementation.
# Input
vectorA :: Vector Floating (vector representing a vector in R4)
vectorB :: Vector Floating (vector representing a vector in R4)
# Output
distance :: Floating (Distance between vectorA and vectorB)
-}
vectorsEuclideanDistance :: (Floating a) => Vector a -> Vector a -> a
vectorsEuclideanDistance (Vector a) (Vector b) =
    sqrt $ sumElements $ ((Vector a) `vectorSubtraction` (Vector b)) ^^^^ 2


