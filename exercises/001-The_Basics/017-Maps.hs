-- A map executes an operation in every element of a list and returns the list
incrementsTwo x = x + 2
main = do 
    let list = map (incrementsTwo) [1, 2, 3, 4, 5]
    print list
    -- output: [3,4,5,6,7]