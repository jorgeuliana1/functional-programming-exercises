-- The ++ operator
counting = [1, 2, 3, 4, 5] ++ [6, 7, 8, 9, 10] -- It merges two lists
-- The : operator
counting_again = 0:counting -- It appends a new value to the list
-- couting_again == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- The !! operator
num = counting !! 1 -- It returns the element on the index 1
-- Note that Haskell counts the indexes starting at 0
main = 
    do print num
       print counting_again