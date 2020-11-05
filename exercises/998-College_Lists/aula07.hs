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
selectInternal :: Int -> Int -> [a] -> a
selectInternal i c (x:xs)
    | c == i = x
    | otherwise = selectInternal i (c + 1) xs

select :: Int -> [a] -> a
select i xs = selectInternal i 0 xs

-- d. Verify if an element E is in the list XS
isIn :: (Eq a) => a -> [a] -> Bool
isIn a [] = False
isIn a (x:xs)
    | a == x = True
    | otherwise = isIn a xs

--isIn :: a -> [a] -> Bool
