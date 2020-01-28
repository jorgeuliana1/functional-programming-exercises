getFibo num = -- This function returns a list containing the fibonacci sequence numbers in order
    -- This function is recursive
    -- Conditional structures are like this:
    if num == 0 then [1]
        else if num == 1 then [1, 1]
            else do
                let pastFibo = getFibo(num - 1)
                let fiboLen = length pastFibo
                pastFibo ++ [(pastFibo !! (fiboLen - 1) + pastFibo !! (fiboLen - 2))]

main =
    do let x = getFibo 10
       print x