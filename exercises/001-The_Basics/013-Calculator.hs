-- I just implemented a simple calculator using guards

calculator :: Float -> Char -> Float -> Float
calculator n1 op n2
    | op == '+' = n1 + n2 -- equivalento to: if op == '+' then n1 + n2
    | op == '-' = n1 - n2
    | op == '*' = n1 * n2
    | op == '/' = n1 / n2
    | otherwise = 0

main = do
    print(calculator 1 '+' 2) -- output: 3.0
    print(calculator 1 '*' 2) -- output: 2.0
    print(calculator 1 '-' 2) -- output: -1.0
    print(calculator 1 '/' 2) -- output: 0.5