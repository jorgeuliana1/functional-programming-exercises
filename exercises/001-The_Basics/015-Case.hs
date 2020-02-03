-- Here we are going to explore the 'case' syntax

lastElement :: [Int] -> Int
lastElement(xs) = case xs of [] -> error "Empty list."
                             (xs) -> xs !! (length xs - 1)
                        
main = print(lastElement [1, 2, 3, 4, 5]) -- output: 5