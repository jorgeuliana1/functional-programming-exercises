module TupleMath where

{-
Sums every value of a list of quadruples and returns the resulting quadruple.
# Input
originTuples :: [(a, a, a, a)] (List of tuples to perform the sum)
# Output
resultingTuple :: (a, a, a, a) (The resulting tuple, with the sums of
                                each "index" of the tuple)
-}
sumQuadruples :: (Num a) => [(a, a, a, a)] -> (a, a, a, a)
sumQuadruples originTuples =
    ( sum [ v | (v, _, _, _) <- originTuples ],
      sum [ v | (_, v, _, _) <- originTuples ],
      sum [ v | (_, _, v, _) <- originTuples ],
      sum [ v | (_, _, _, v) <- originTuples ] )

{-
Returns the sum of the elements of the quadruple.
# Input
quadruple :: [(a, a, a, a)] (List of tuples to perform the sum)
# Output
result :: a (The result of the sum of elements of the tuple)
-}
sumElements :: (Num a) => (a, a, a, a) -> a
sumElements quadruple = a + b + c + d
    where
        (a, b, c, d) = quadruple

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
(////) :: (Fractional a) => (a, a, a, a) -> a -> (a, a, a, a)
quadruple //// divisor = (v1 / divisor, v2 / divisor, v3 / divisor, v4 / divisor)
    where
        (v1, v2, v3, v4) = quadruple

{-
Works as "for-each" power function for quadruples.
# Input
quadruple :: (a, a, a, a) (Tuple to be powered)
power :: Int (The power of the operation)
# Ouput
resultingQuadruple :: (a, a, a, a) (The resulting quadruple)
-}
(^^^^) :: (Num a) => (a, a, a, a) -> Int -> (a, a, a, a)
quadruple ^^^^ power = (v1^power, v2^power, v3^power, v4^power)
    where
        (v1, v2, v3, v4) = quadruple

{-
Works as "for-each" sqrt function for quadruples.
# Input
quadruple :: (a, a, a, a) (Tuple to be square-rooted)
# Ouput
resultingQuadruple :: (a, a, a, a) (The square-rooted quadruple)
-}
tupleSqrt :: (Floating a) => (a, a, a, a) -> (a, a, a, a)
tupleSqrt quadruple = (sqrt v1, sqrt v2, sqrt v3, sqrt v4)
    where
        (v1, v2, v3, v4) = quadruple

{-
Uses the euclidean length to find the vector (tuple) with least magnitude.
# Input
quadruples :: [(a, a, a, a)] (List of tuples to be analyzed)
# Ouput
resultingQuadruple :: (a, a, a, a) (The tuple with least magnitude in the
                                    given list)
-}
tupleMinimum :: [(Float, Float, Float, Float)] -> (Float, Float, Float, Float)
tupleMinimum quadruples = [ quadruples !! i | i <- [0..(length quadruples) - 1], equivalentValue (quadruples !! i) == minEucLength ] !! 0
    where
        minEucLength = minimum equivalentValues -- Minimum Euclidean Length
        equivalentValues = [ equivalentValue quadruple | quadruple <- quadruples ]
        equivalentValue quadruple = sqrt (a^2 + b^2 + c^2 + d^2) -- Euclidean length
            where
                (a, b, c, d) = quadruple

{-
Works as "for-each" subtraction for quadruples.
# Input
quadrupleA :: (a, a, a, a) (First member of the subtraction operation)
quadrupleB :: (a, a, a, a) (Second member of the subtraction operation)
# Ouput
resultingQuadruple :: (a, a, a, a) (The result of the subtraction)
-}
tupleSubtraction :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
tupleSubtraction quadrupleA quadrupleB = (a1 - b1, a2 - b2, a3 - b3, a4 - b4)
    where
        (a1, a2, a3, a4) = quadrupleA
        (b1, b2, b3, b4) = quadrupleB

{-
Works as "for-each" abs for quadruples.
# Input
quadruple :: (a, a, a, a) (Input tuple)
# Ouput
resultingQuadruple :: (a, a, a, a) (A quadruple containing the absolute
                                    values of the input tuple)
-}
tupleAbs :: (Num a) => (a, a, a, a) -> (a, a, a, a)
tupleAbs quadruple = (abs v1, abs v2, abs v3, abs v4)
    where
        (v1, v2, v3, v4) = quadruple

{-
Classical euclidean distance implementation.
# Input
quadrupleA :: (a, a, a, a) (Quadruple representing a vector in R4)
quadrupleB :: (a, a, a, a) (Quadruple representing a vector in R4)
# Output
distance :: a (Distance between quadrupleA and quadrupleB)
-}
euclideanDistance :: (Floating a) => (a, a, a, a) -> (a, a, a, a) -> a
euclideanDistance quadrupleA quadrupleB =
    sqrt (sumElements ((quadrupleA `tupleSubtraction` quadrupleB) ^^^^ 2))