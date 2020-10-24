module TupleMath where

-- Sums every value of a list of quadruples and returns the resulting quadruple
sumQuadruples :: (Num a) => [(a, a, a, a)] -> (a, a, a, a)
sumQuadruples originTuple =
    ( sum [ v | (v, _, _, _) <- originTuple ],
      sum [ v | (_, v, _, _) <- originTuple ],
      sum [ v | (_, _, v, _) <- originTuple ],
      sum [ v | (_, _, _, v) <- originTuple ] )

-- Works as "for-each" division for quadruples.
(////) :: (Fractional a) => (a, a, a, a) -> a -> (a, a, a, a)
quadruple //// divider = (v1 / divider, v2 / divider, v3 / divider, v4 / divider)
    where
        (v1, v2, v3, v4) = quadruple

-- Works as "for-each" power function for quadruples.
(^^^^) :: (Num a) => (a, a, a, a) -> Int -> (a, a, a, a)
quadruple ^^^^ power = (v1^power, v2^power, v3^power, v4^power)
    where
        (v1, v2, v3, v4) = quadruple

-- Works as "for-each" sqrt function for quadruples.
tupleSqrt :: (Floating a) => (a, a, a, a) -> (a, a, a, a)
tupleSqrt quadruple = (sqrt v1, sqrt v2, sqrt v3, sqrt v4)
    where
        (v1, v2, v3, v4) = quadruple

-- Uses the euclidean length to find the vector (tuple) with least magnitude.
tupleMinimum :: [(Float, Float, Float, Float)] -> (Float, Float, Float, Float)
tupleMinimum quadruples = [ quadruples !! i | i <- [0..(length quadruples) - 1], equivalentValue (quadruples !! i) == minEucLength ] !! 0
    where
        minEucLength = minimum equivalentValues -- Minimum Euclidean Length
        equivalentValues = [ equivalentValue quadruple | quadruple <- quadruples ]
        equivalentValue quadruple = sqrt (a^2 + b^2 + c^2 + d^2) -- Euclidean length
            where
                (a, b, c, d) = quadruple

-- Works as "for-each" subtraction for quadruples.
tupleSubtraction :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
tupleSubtraction quadrupleA quadrupleB = (a1 - b1, a2 - b2, a3 - b3, a4 - b4)
    where
        (a1, a2, a3, a4) = quadrupleA
        (b1, b2, b3, b4) = quadrupleB

-- Works as "for-each" abs for quadruples
tupleAbs :: (Num a) => (a, a, a, a) -> (a, a, a, a)
tupleAbs quadruple = (abs v1, abs v2, abs v3, abs v4)
    where
        (v1, v2, v3, v4) = quadruple