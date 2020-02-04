-- Predicate is a function that tells whether something is true or not
-- In our case, a predicate returns a boolean value
-- predicate :: a -> Bool
isEight :: (Integral a) => a-> Bool -- isEight is a predicate
isEight x = x == 8

main = print(isEight 8)
-- output: True