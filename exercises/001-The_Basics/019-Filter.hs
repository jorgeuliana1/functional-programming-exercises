-- We are gonna recreate our predicate function here
isEight :: (Integral a) => a -> Bool
isEight n = n == 8

-- Now, we are gonna create a list that contains many numbers
xs = [1, 2, 3, 4, 5, 8, 1, 8, 2, 8, 3, 8]

-- Let's use the filter function to create a list containing
--  only the 8s of xs
xf = filter (isEight) xs

main = print xf
-- output: [8,8,8,8]