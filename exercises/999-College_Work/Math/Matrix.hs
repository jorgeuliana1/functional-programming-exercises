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