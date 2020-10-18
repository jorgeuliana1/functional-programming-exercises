main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("The reverse of " ++ name ++ " is " ++ (reverse name))