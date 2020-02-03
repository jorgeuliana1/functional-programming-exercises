-- We declare an empty list as:
empty_list = []
-- We can declare a list with content as:
full_list = [1, 2, 3, 4, 5, 6]
-- In Haskell, lists are homogeneous
-- Also, strings are lists of characters

-- We can declare strings:
foobar_list = ['f', 'o', 'o', 'b', 'a', 'r']
-- or
foobar_str = "foobar"

-- Comparing the result:
main = print(foobar_list == foobar_str) -- This returns true