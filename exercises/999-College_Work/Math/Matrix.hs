module Math.Matrix where
import Math.Vector

{--
This module defines the matrix abstract data type. In the given definition, a matrix is a set of vectors.
--}

data Matrix a = Matrix [Vector a] deriving (Eq, Show)

{-
Returns the resulting matrix of the sum of two matrixes.
# Input
a :: Matrix Num (The first matrix of the operation)
b :: Matrix Num (The second matrix of the operation)
# Output
result :: Matrix Num (The resulting matrix)
-}
addMatrixes :: (Num a) => Matrix a -> Matrix a -> Matrix a
(Matrix a) `addMatrixes` (Matrix b) = Matrix $ zipWith addVectors a b

{-
Returns a vector of the simple average values of a matrix.
# Input
m :: Matrix Double (Matrix to be analyzed)
# Ouput
result :: Vector Double (Average vector)
-}
matrixSimpleAverage :: Matrix Double -> Vector Double
matrixSimpleAverage (Matrix m) = (sumVectors m) //// l
    where
        l = read (show $ length m) :: Double

{-
Returns a vector of the variance of a matrix.
# Input
m :: Matrix Double (Matrix to be analyzed)
# Output
result :: Vector Double (Variance vector)
-}
matrixVariance :: Matrix Double -> Vector Double
matrixVariance (Matrix m) = (sumVectors $ map (\xi -> (xi `vectorSubtraction` mi)^^^^2) m) //// l
    where
        l = read (show $ length m) :: Double
        mi = matrixSimpleAverage (Matrix m)

{-
Returns a vector of the standard deviation of a matrix.
# Input
m :: Matrix Double (Matrix to be analyzed)
# Output
result :: Vector Double (Standard Deviation vector)
-}
matrixStandardDeviation :: Matrix Double -> Vector Double
matrixStandardDeviation (Matrix m) = vectorSqrt $ matrixVariance (Matrix m)

{-
Normalize the matrix.
# Input
m :: Matrix Double (Matrix to be normalized)
# Output
z :: Matrix Double (Normalized matrix)
-}
matrixNormalize :: Matrix Double -> Matrix Double
matrixNormalize (Matrix m) = Matrix $ map (\x -> (x `vectorSubtraction` mi) `vectorDiv` sigma) m
    where
        mi = matrixSimpleAverage (Matrix m)
        sigma = matrixStandardDeviation (Matrix m)