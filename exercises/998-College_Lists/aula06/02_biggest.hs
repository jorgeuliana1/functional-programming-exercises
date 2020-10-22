-- Returns the biggest number of a given list
getInputList :: IO [Float]
getInputList = do
    nm <- getLine
    return (map (read :: String -> Float) (words nm))
                
main = do
    putStrLn "Give the list of numbers:"
    xs <- getInputList
    print (maximum xs)