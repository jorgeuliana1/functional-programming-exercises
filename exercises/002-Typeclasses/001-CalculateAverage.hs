-- Calculating the average of a list of numbers
-- The number must be a 'Fractional'

-- Note that we can't divide real numbers in Haskell.
-- The number must have the Fractional instance to perform / 

listAverage :: (Fractional a) => [a] -> a
listAverage xs = sum xs / fromIntegral (length xs)