getFibo num -- This function returns a list containing the fibonacci sequence numbers in order
    -- This function is recursive
    -- Conditional structures are like this:
    | num == 0 = [1]
    | num == 1 = [1, 1]
    | otherwise = fibo
    where
        pastFibo = getFibo(num - 1)
        fiboLen = length pastFibo
        fibo = pastFibo ++ [(pastFibo !! (fiboLen - 1) + pastFibo !! (fiboLen - 2))]

main = print(getFibo 10)