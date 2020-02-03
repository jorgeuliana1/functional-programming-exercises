-- The ':t' expression tells us the type of the expression
-- '::' is read as 'has type of'

-- Declaring a function with type declaration:
onlyEvens :: [Int] -> [Int]
-- This means that the function 'onlyEvens' receives integers list and returns integers list
getSecond :: [Int] -> Int
-- getSecond receives integers list and returns integer

onlyEvens ns = [c | c <- ns, even c]
-- Returns a list of integers of even numbers in a given interval

getSecond ns = ns !! 1
-- Returns the second element of a list
main = do
            print (onlyEvens [0..10]) -- returns [0, 2, 4, 6, 8, 10]
            print (getSecond [1..10]) -- returns 2