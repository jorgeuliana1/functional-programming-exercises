-- a. Concatenate a list of lists
concatenateLists :: [[a]] -> Int -> Int -> [a]
concatenateLists xss xssIndex xssLen
    | xssIndex /= xssLen = (xss !! (xssIndex - 1)) ++ (concatenateLists xss (xssIndex + 1) xssLen)
    | otherwise = xss !! (xssLen - 1)

concatenateListsWrapped :: [[a]] -> [a]
concatenateListsWrapped xss = concatenateLists xss 1 (length xss)

-- b. Replicate the same element many times in a list
replica :: Int -> a -> [a]
replica r e
    | r > 1 = e:(replicate (r - 1) e)
    | otherwise = [e]

-- c. Select the n-esimal element of a list
-- select :: Int -> [a] -> a