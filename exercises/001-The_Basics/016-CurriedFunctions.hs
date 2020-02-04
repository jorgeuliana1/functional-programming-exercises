-- A function can be created with base in another function
simpleSum :: Int -> Int -> Int -> Int
-- simpleSum sums three numbers
simpleSum a b c = a + b + c

-- We can create sumToThree
sumToThree = simpleSum 3
-- and see the result
main = print(sumToThree 2 1) -- 3 + 2 + 1
-- output: 6