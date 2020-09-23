add :: Integral a => a -> a -> a
add x y = x + y

evens :: Integral a => a -> a -> [a]
evens i j = [n | n <- [i..j], even n]

half :: [a] -> Int
half xs = (length xs) `div` 2
