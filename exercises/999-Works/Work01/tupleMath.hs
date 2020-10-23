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