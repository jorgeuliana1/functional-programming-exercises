showCommands :: IO ()
showCommands = do
    putStrLn "What is your option?"
    putStrLn "0: EXIT"
    putStrLn "1: SUM"
    putStrLn "2: SUBTRACT"
    putStrLn "3: MULTIPLY"
    putStrLn "4: DIVIDE"

operateNumbers :: Float -> Int -> Float -> Float
operateNumbers num1 op num2
    | op == 1 = num1 + num2
    | op == 2 = num1 - num2
    | op == 3 = num1 * num2
    | op == 4 = num1 / num2

operationLoop :: Int -> IO ()
operationLoop 0 = return ()
operationLoop op = do
    putStr "First number: "
    num1 <- getLine
    putStr "Second number: "
    num2 <- getLine
    print (operateNumbers (read num1 :: Float) op (read num2 :: Float))
    calculatorExecutionLoop

calculatorExecutionLoop :: IO ()
calculatorExecutionLoop = do
    showCommands
    op <- getLine
    operationLoop (read op :: Int)

main = do
   calculatorExecutionLoop