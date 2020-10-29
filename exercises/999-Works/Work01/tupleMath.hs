module TupleMath where

{-
Sums every value of a list of quadruples and returns the resulting quadruple.
# Input
originTuples :: [(a, a, a, a)] (List of tuples to perform the sum)
# Output
resultingTuple :: (a, a, a, a) (The resulting tuple, with the sums of
                                each "index" of the tuple)
-}
addVectors :: (Num a) => [a] -> [a] -> [a]
addVectors [] vectorB = vectorB
addVectors vectorA [] = vectorA
addVectors vectorA vectorB = [ (vectorA !! i) + (vectorB !! i) | i <- [0..(length vectorA) - 1] ]

sumVectors :: (Num a) => [[a]] -> [a]
sumVectors [] = []
sumVectors (componentA:vector) = componentA `addVectors` (sumVectors vector)

{-
Returns the sum of the elements of the quadruple.
# Input
quadruple :: [(a, a, a, a)] (List of tuples to perform the sum)
# Output
result :: a (The result of the sum of elements of the tuple)
-}
sumElements :: (Num a) => [a] -> a
sumElements quadruple = sum quadruple

{-
Works as "for-each" division for quadruples.
# Input
quadruple :: (a, a, a, a) (Tuple to be divided)
divisor :: a (Divisor of the elements of the tuple)
# Output
resultingTuple :: (a, a, a, a) (The resulting tuple, with each element
                                of the original tuple divided by the
                                given divisor)
-}
(////) :: (Fractional a) => [a] -> a -> [a]
quadruple //// divisor = [ value / divisor | value <- quadruple ]

{-
Works as "for-each" power function for quadruples.
# Input
quadruple :: (a, a, a, a) (Tuple to be powered)
power :: Int (The power of the operation)
# Ouput
resultingQuadruple :: (a, a, a, a) (The resulting quadruple)
-}
(^^^^) :: (Num a) => [a] -> Int -> [a]
quadruple ^^^^ power = [ v ^ power | v <- quadruple]

{-
Works as "for-each" sqrt function for quadruples.
# Input
quadruple :: (a, a, a, a) (Tuple to be square-rooted)
# Ouput
resultingQuadruple :: (a, a, a, a) (The square-rooted quadruple)
-}
tupleSqrt :: (Floating a) => [a] -> [a]
tupleSqrt quadruple = [ sqrt v | v <- quadruple ]

{-
Works as "for-each" subtraction for quadruples.
# Input
quadrupleA :: (a, a, a, a) (First member of the subtraction operation)
quadrupleB :: (a, a, a, a) (Second member of the subtraction operation)
# Ouput
resultingQuadruple :: (a, a, a, a) (The result of the subtraction)
-}
tupleSubtraction :: (Num a) => [a] -> [a] -> [a]
tupleSubtraction quadrupleA quadrupleB = [ (quadrupleA !! i) - (quadrupleB !! i) | i <- [0..(length quadrupleA) - 1]]

{-
Works as "for-each" abs for quadruples.
# Input
quadruple :: (a, a, a, a) (Input tuple)
# Ouput
resultingQuadruple :: (a, a, a, a) (A quadruple containing the absolute
                                    values of the input tuple)
-}
tupleAbs :: (Num a) => [a] -> [a]
tupleAbs quadruple = [abs v | v <- quadruple]

{-
Classical euclidean distance implementation.
# Input
quadrupleA :: (a, a, a, a) (Quadruple representing a vector in R4)
quadrupleB :: (a, a, a, a) (Quadruple representing a vector in R4)
# Output
distance :: a (Distance between quadrupleA and quadrupleB)
-}
euclideanDistance :: (Floating a) => [a] -> [a] -> a
euclideanDistance quadrupleA quadrupleB =
    sqrt (sumElements ((quadrupleA `tupleSubtraction` quadrupleB) ^^^^ 2))